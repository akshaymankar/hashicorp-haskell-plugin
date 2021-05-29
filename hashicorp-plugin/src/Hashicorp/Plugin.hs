{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hashicorp.Plugin where

import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Attoparsec.Text as A
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified GRpc.Health.V1 as Health
import qualified Hashicorp.GRpc.Broker as Broker
import qualified Hashicorp.GRpc.Controller as Controller
import Mu.GRpc.Server (GRpcMessageProtocol (..), WrappedServer (Srv), gRpcMultipleAppTrans, msgProtoBuf)
import Mu.Server (ServerError, ServerErrorIO)
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket)
import qualified System.Environment as OS
import System.Exit (ExitCode (ExitFailure))
import qualified System.Exit as System
import System.IO (hFlush, stderr, stdout)
import qualified System.IO.Temp as Temp

data HandshakeConfig = HandshakeConfig
  { magicCookieKey :: String,
    magicCookieValue :: String
  }
  deriving (Show, Eq)

newtype Plugin = Plugin [WrappedServer 'MsgProtoBuf PluginT]

newtype PluginT a = PluginT
  { unPluginT ::
      ReaderT
        ( TVar Health.HealthMap,
          TVar Broker.Connections,
          MVar Controller.ExitSignal
        )
        ServerErrorIO
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError ServerError,
      MonadReader
        ( TVar Health.HealthMap,
          TVar Broker.Connections,
          MVar Controller.ExitSignal
        )
    )

runPluginT ::
  TVar Health.HealthMap ->
  TVar Broker.Connections ->
  MVar Controller.ExitSignal ->
  PluginT a ->
  ServerErrorIO a
runPluginT health broker exit = flip runReaderT (health, broker, exit) . unPluginT

data ServeConfig = ServeConfig
  { handshakeConfig :: HandshakeConfig,
    versionedPluginSet :: HashMap Version Plugin
  }

data Protocol
  = ProtocolRPC
  | ProtocolGRPC
  deriving (Eq)

instance Show Protocol where
  show ProtocolGRPC = "grpc"
  show ProtocolRPC = "netrpc"

type Version = Int

serve :: ServeConfig -> IO ()
serve cfg@ServeConfig {..} = do
  unless
    (isMagicConfigured handshakeConfig)
    (printErrorAndExit noMagicError (ExitFailure 1))
  providedMagicCookieValue <- OS.lookupEnv (magicCookieKey handshakeConfig)

  when
    (providedMagicCookieValue /= Just (magicCookieValue handshakeConfig))
    (printErrorAndExit wrongMagicError (ExitFailure 1))
  -- TODO: Detect windows and listen on TCP instead
  sock <- listenUnix
  let protocol = ProtocolGRPC -- TODO: Make this configurable
      cert = "" -- TODO: Use TLS
  (version, plugin) <- selectPlugins cfg
  printListener version sock protocol cert
  startServing sock plugin

isMagicConfigured :: HandshakeConfig -> Bool
isMagicConfigured HandshakeConfig {..} = not (null magicCookieKey || null magicCookieValue)

startServing :: Network.Socket -> Plugin -> IO ()
startServing sock (Plugin pluginServers) = do
  health <- newTVarIO =<< Health.newHealthMap
  broker <- newTVarIO mempty
  exit <- newEmptyMVar
  flip runReaderT health $ Health.setServingStatus "plugin" Health.ServingStatusServing
  let servers =
        Srv Controller.grpcControllerServer :
        Srv Broker.grpcBrokerServer :
        Srv Health.healthServer :
        pluginServers
      app = gRpcMultipleAppTrans msgProtoBuf (runPluginT health broker exit) servers
  race_ (takeMVar exit) $ runSettingsSocket defaultSettings sock app

coreProtocolVersion :: Integer
coreProtocolVersion = 1

printListener ::
  Version ->
  Network.Socket ->
  Protocol ->
  -- | Base64 encoded server certificate without newlines
  String ->
  IO ()
printListener v sock proto serverCert = do
  socketFamily <- showSocketFamily <$> getSocketFamily sock
  addr <- Network.getSocketName sock
  putStrLn $
    List.intercalate
      "|"
      [ show coreProtocolVersion,
        show v,
        socketFamily,
        show addr,
        show proto,
        serverCert
      ]
  hFlush stdout
  where
    getSocketFamily s = Network.unpackFamily <$> Network.getSockOpt s Network.SoDomain
    showSocketFamily = \case
      Network.AF_UNIX -> "unix"
      Network.AF_INET -> "tcp"
      _ -> error "unhandled socket family"

listenTCP :: IO Network.Socket
listenTCP = error "Not implemented, to be used on windows"

listenUnix :: IO Network.Socket
listenUnix = do
  pluginDir <- flip Temp.createTempDirectory "hashicorp-plugin" =<< Temp.getCanonicalTemporaryDirectory
  sock <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
  Network.bind sock $ Network.SockAddrUnix $ pluginDir <> "/sock"
  Network.listen sock 5
  pure sock

selectPlugins :: ServeConfig -> IO (Version, Plugin)
selectPlugins ServeConfig {..} = matchingVersion versionedPluginSet <$> clientVersions

clientVersions :: IO [Version]
clientVersions =
  OS.lookupEnv "PLUGIN_PROTOCOL_VERSIONS" >>= \case
    Nothing -> pure []
    Just verStr ->
      case A.parseOnly clientVersionsParser (Text.pack verStr) of
        Left err -> printError ("server sent invalid plugin version: " <> Text.pack err) >> pure []
        Right x -> pure x

matchingVersion :: HashMap Version a -> [Version] -> (Version, a)
matchingVersion plugins versions =
  let availableVersions = Map.keysSet plugins
      matchingVersions = Set.intersection availableVersions (Set.fromList versions)
      chosenVersion =
        if Set.null matchingVersions
          then minimum availableVersions -- TODO: This could be empty!
          else maximum matchingVersions
   in (chosenVersion, (Map.!) plugins chosenVersion)

clientVersionsParser :: A.Parser [Version]
clientVersionsParser = A.decimal `A.sepBy` A.char ','

printErrorAndExit :: Text -> ExitCode -> IO ()
printErrorAndExit err code = do
  Text.hPutStrLn stderr err
  System.exitWith code

printError :: Text -> IO ()
printError = Text.hPutStrLn stderr

noMagicError :: Text
noMagicError =
  "Misconfigured ServeConfig given to serve this plugin: no magic cookie\n"
    <> "key or value was set. Please notify the plugin author and report\n"
    <> "this as a bug.\n"

wrongMagicError :: Text
wrongMagicError =
  "This binary is a plugin. These are not meant to be executed directly.\n"
    <> "Please execute the program that consumes these plugins, which will\n"
    <> "load any plugins automatically\n"
