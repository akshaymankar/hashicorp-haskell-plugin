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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hashicorp.Plugin where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Attoparsec.Text as A
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.Kind (Type)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Fcf (Eval)
import Fcf.Data.List (type (++))
import GHC.TypeLits (Symbol)
import qualified GRpc.Health.V1 as Health
import qualified Hashicorp.GRpc.Broker as Broker
import Mu.GRpc.Server (GRpcMessageProtocol (..), GRpcServerHandlers, gRpcServerHandlers, msgProtoBuf)
import Mu.Rpc (Package, TypeRef)
import Mu.Server (ServerError, ServerErrorIO, ServerT (NoPackages, Packages), SingleServerT)
import Network.GRPC.HTTP2.Encoding (gzip, uncompressed)
import qualified Network.GRPC.Server as Wai
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

type PluginServer = ServerT

newtype
  Plugin
    (pkgs :: [Package Symbol Symbol Symbol (TypeRef Symbol)])
    (hs :: [[[Type]]]) = Plugin {pluginServer :: PluginServer '[] () pkgs PluginT hs}

newtype PluginT a = PluginT {unPluginT :: ReaderT (TVar Health.HealthMap, TVar Broker.Connections) ServerErrorIO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError ServerError,
      MonadReader (TVar Health.HealthMap, TVar Broker.Connections)
    )

data ServeConfig pkgs hs = ServeConfig
  { handshakeConfig :: HandshakeConfig,
    versionedPluginSet :: HashMap Version (Plugin pkgs hs)
  }

data Protocol
  = ProtocolRPC
  | ProtocolGRPC
  deriving (Eq)

instance Show Protocol where
  show ProtocolGRPC = "grpc"
  show ProtocolRPC = "netrpc"

type Version = Int

serve ::
  (GRpcServerHandlers pkgs MsgProtoBuf PluginT '[] hs) =>
  ServeConfig pkgs hs ->
  IO ()
serve cfg@ServeConfig {..} = do
  when
    (not $ isMagicConfigured handshakeConfig)
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

startServing ::
  forall pkgs hs.
  (GRpcServerHandlers pkgs MsgProtoBuf PluginT '[] hs) =>
  Network.Socket ->
  Plugin pkgs hs ->
  IO ()
startServing sock (Plugin server) = do
  health <- newTVarIO =<< Health.newHealthMap
  broker <- newTVarIO mempty
  flip runReaderT health $ (Health.setServingStatus "plugin" Health.ServingStatusServing)
  let servers = combineServers Broker.grpcBrokerServer $ combineServers Health.healthServer server
      handlers = gRpcServerHandlers (flip runReaderT (health, broker) . unPluginT) msgProtoBuf servers
      app =
        Wai.grpcApp
          [uncompressed, gzip]
          handlers
  runSettingsSocket defaultSettings sock app

combineServers ::
  SingleServerT () pkg1 m hs1 ->
  ServerT '[] () restPkgs m restHs ->
  ServerT '[] () (pkg1 ': restPkgs) m (Eval (hs1 ++ restHs))
combineServers (Packages p1 NoPackages) s2 = Packages p1 s2

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

selectPlugins :: ServeConfig s hs -> IO (Version, Plugin s hs)
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
clientVersionsParser = A.decimal `A.sepBy` (A.char ',')

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
