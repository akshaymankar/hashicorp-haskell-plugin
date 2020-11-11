{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Hashicorp.GRpc.Controller where

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Has (Has, getter)
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (FromSchema, ToSchema)
import Mu.Server (MonadServer, SingleServerT, method, singleService)

grpc "GRPCControllerSchema" id "grpc_controller.proto"

deriving instance ToSchema GRPCControllerSchema "Empty" ()

deriving instance FromSchema GRPCControllerSchema "Empty" ()

type ExitSignal = ()

shutdown :: (MonadIO m, MonadReader r m, Has (MVar ExitSignal) r) => () -> m ()
shutdown _ =
  liftIO . flip putMVar () =<< asks getter

grpcControllerServer :: (MonadServer m, MonadIO m, MonadReader r m, Has (MVar ()) r) => SingleServerT info GRPCController m _
grpcControllerServer = singleService (method @"Shutdown" shutdown)
