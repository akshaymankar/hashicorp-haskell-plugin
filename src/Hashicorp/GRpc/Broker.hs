{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Hashicorp.GRpc.Broker where

import Conduit (ConduitT, Void, (.|))
import qualified Conduit as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TMVar, TVar, atomically, modifyTVar, newEmptyTMVar, newTMVar, putTMVar, readTMVar, readTVar, readTVarIO, swapTMVar, takeTMVar, tryReadTMVar)
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (Bifunctor (first))
import Data.Has (Has, getter)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Debug.Trace (traceShowM)
import GHC.Generics (Generic)
import Mu.Quasi.GRpc (grpc)
import Mu.Schema
import Mu.Server (MonadServer, SingleServerT, method, singleService)
import Network.GRPC.Client (Timeout (Timeout), uncompressed)
import Network.GRPC.Client.Helpers
import Network.HTTP2.Client (ignoreFallbackHandler)

grpc "GRPCBrokerSchema" id "grpc_broker.proto"

type ConnInfoFieldMapping = '["serviceId" :-> "service_id"]

data ConnInfo = ConnInfo
  { serviceId :: Word32,
    address :: Text,
    network :: Text
  }
  deriving (Show, Eq, Generic)
  deriving
    (ToSchema GRPCBrokerSchema "ConnInfo", FromSchema GRPCBrokerSchema "ConnInfo")
    via (CustomFieldMapping "ConnInfo" ConnInfoFieldMapping ConnInfo)

type Connections = HashMap Word32 (TMVar ConnInfo)

-- Host talks to Plugin  over connection info from plugin's stdout
-- When host wants to allow plugin to talk, it starts a server, sends details to plugin via grpc broker server `startStream` with conn details. Plugin remembers this
-- When plugin wants to talk to host, it checks if there is a stream from host, if there is, uses that to talk

-- Job of this function is to send nothing and only recieve. The send will be required if this library implements hosting a plugin
startStream ::
  (MonadIO m, MonadReader r m, Has (TVar Connections) r, MonadServer m) =>
  ConduitT () ConnInfo m () ->
  ConduitT ConnInfo Void m () ->
  m ()
startStream source _ = do
  C.runConduit $
    source
      .| C.mapM_C
        ( \newConn -> do
            connsTVar <- asks getter
            liftIO . atomically $ do
              conns <- readTVar connsTVar
              case Map.lookup (serviceId newConn) conns of
                Nothing -> do
                  newConnTMVar <- newTMVar newConn
                  modifyTVar connsTVar (Map.insert (serviceId newConn) newConnTMVar)
                Just connTMVar -> do
                  tryReadTMVar connTMVar >>= \case
                    Nothing -> putTMVar connTMVar newConn
                    Just _ -> void $ swapTMVar connTMVar newConn
        )

getClientById :: (MonadReader r m, Has (TVar Connections) r, MonadIO m) => Word32 -> m (Either String GrpcClient)
getClientById requestedServiceId = do
  connsTVar :: TVar Connections <- asks getter
  connTMVar <- liftIO . atomically $ do
    conns <- readTVar connsTVar
    case Map.lookup requestedServiceId conns of
      Nothing -> do
        newConnTMVar <- newEmptyTMVar
        modifyTVar connsTVar (Map.insert requestedServiceId newConnTMVar)
        pure newConnTMVar
      Just connTMVar -> pure connTMVar
  ConnInfo {..} <- liftIO . atomically $ takeTMVar connTMVar
  let timeout = Timeout 3000
      tls = Nothing
  addr <- case network of
    "unix" -> pure $ AddressUnix (Text.unpack address)
    "tcp" ->
      case Text.split (== ':') address of
        [host, port] -> pure $ AddressTCP (Text.unpack host) (read (Text.unpack port))
        _ -> error $ "Invalid TCP Address: " <> show address -- TODO: Make this pure
    _ -> error $ "Invalid Network: " <> show network
  let config = GrpcClientConfig addr [] timeout uncompressed tls (liftIO . throwIO) ignoreFallbackHandler 5000000 1000000
  liftIO $
    fmap (first show)
      . runExceptT
      . setupGrpcClient
      $ config

grpcBrokerServer :: (MonadIO m, MonadServer m, MonadReader r m, Has (TVar Connections) r) => SingleServerT info GRPCBroker m _
grpcBrokerServer = singleService (method @"StartStream" startStream)
