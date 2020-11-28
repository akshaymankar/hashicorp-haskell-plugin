{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module GRpc.Health.V1
  ( HealthMap,
    newHealthMap,
    setServingStatus,
    healthServer,
    ServingStatus (..),
    Health (..),
  )
where

import BroadcastChan (BroadcastChan, In, Out, newBChanListener, newBroadcastChan, readBChan)
import Conduit (ConduitT, (.|))
import qualified Conduit as C
import Control.Concurrent.STM (TVar, atomically, readTVarIO, swapTVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Conduit.List as C
import Data.Has (Has, getter)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (CustomFieldMapping (..), FromSchema, Mapping ((:->)), ToSchema)
import Mu.Server (MonadServer, ServerError (ServerError), ServerErrorCode (NotFound), SingleServerT, method, serverError, singleService)

grpc "HealthSchema" id "health.proto"

newtype HealthCheckRequest = HealthCheckRequest {service :: Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema HealthSchema "HealthCheckRequest",
      FromSchema HealthSchema "HealthCheckRequest"
    )

type ServingStatusFieldMapping =
  '[ "ServingStatusUnknown" :-> "UNKNOWN",
     "ServingStatusServing" :-> "SERVING",
     "ServingStatusNotServing" :-> "NOT_SERVING",
     "ServingStatusServiceUnknown" :-> "SERVICE_UNKNOWN"
   ]

data ServingStatus
  = ServingStatusUnknown
  | ServingStatusServing
  | ServingStatusNotServing
  | ServingStatusServiceUnknown
  deriving (Show, Eq, Generic)
  deriving
    (ToSchema HealthSchema "HealthCheckResponse.ServingStatus", FromSchema HealthSchema "HealthCheckResponse.ServingStatus")
    via (CustomFieldMapping "HealthCheckResponse.ServingStatus" ServingStatusFieldMapping ServingStatus)

newtype HealthCheckResponse = HealthCheckResponse {status :: Maybe ServingStatus}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema HealthSchema "HealthCheckResponse",
      FromSchema HealthSchema "HealthCheckResponse"
    )

newtype HealthMap = HealthMap {healthMap :: HashMap Text (ServingStatus, BroadcastChan In ServingStatus)}

newHealthMap :: MonadIO m => m HealthMap
newHealthMap = do
  chan <- newBroadcastChan
  pure $ HealthMap $ M.singleton "" (ServingStatusServing, chan)

setServingStatus :: (MonadReader r m, Has (TVar HealthMap) r, MonadIO m) => Text -> ServingStatus -> m ()
setServingStatus name newStatus = do
  mapTVar <- asks getter
  HealthMap {..} <- (liftIO . readTVarIO) mapTVar
  chan <- maybe newBroadcastChan (pure . snd) (M.lookup name healthMap)
  void . liftIO . atomically $ swapTVar mapTVar (HealthMap (M.insert name (newStatus, chan) healthMap))

check :: (MonadServer m, MonadReader r m, Has (TVar HealthMap) r) => HealthCheckRequest -> m HealthCheckResponse
check HealthCheckRequest {..} = do
  HealthMap {..} <- liftIO . readTVarIO =<< asks getter
  pure $ HealthCheckResponse $ fst <$> M.lookup service healthMap

watch :: (MonadServer m, MonadReader r m, Has (TVar HealthMap) r) => HealthCheckRequest -> ConduitT HealthCheckResponse Void m () -> m ()
watch HealthCheckRequest {..} sink = do
  HealthMap {..} <- liftIO . readTVarIO =<< asks getter
  (currentStatus, sourceChan) <- case M.lookup service healthMap of
    Nothing -> serverError (ServerError NotFound ("No service called: " <> show service))
    Just x -> pure x
  listener <- newBChanListener sourceChan
  C.runConduit $
    bChanToConduit listener currentStatus
      .| C.map (HealthCheckResponse . Just)
      .| sink

bChanToConduit :: MonadIO m => BroadcastChan Out a -> a -> ConduitT () a m ()
bChanToConduit listener firstVal = do
  C.yield firstVal
  forever $ do
    maybeVal <- readBChan listener
    case maybeVal of
      Nothing -> pure ()
      Just x -> do
        C.yield x

healthServer :: (MonadServer m, MonadReader r m, Has (TVar HealthMap) r) => SingleServerT info Health m _
healthServer =
  singleService
    ( method @"Check" check,
      method @"Watch" watch
    )
