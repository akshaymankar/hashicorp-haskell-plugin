{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Waypoint.TerminalUI (TerminalT, runTerminalT, MonadTerminal (..)) where

import Conduit (ConduitT, Void)
import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.Trans (MonadTrans)
import qualified Data.Conduit as C
import Data.Has (Has)
import Data.Text (Text)
import qualified Hashicorp.GRpc.Broker as Broker
import Mu.GRpc.Client.Record (CompressMode (Compressed), GRpcMessageProtocol (MsgProtoBuf), GRpcReply (..), GrpcClient)
import Mu.GRpc.Client.TyApps (gRpcCall)
import qualified Waypoint.Inputs as Inputs
import Waypoint.Proto (TerminalUIService)
import Waypoint.TerminalUI.Types (Event, IsInteractiveResponse (interactive), OutputRequest (OutputRequest), Response)
import Control.Monad (void)

class MonadTerminal m where
  output :: [Text] -> m ()
  isInteractive :: m Bool
  events :: m (ConduitT Event Void m (), ConduitT () Response m ())

newtype TerminalConnection = TerminalConnection {client :: GrpcClient}

newtype TerminalT m a = TerminalT (ReaderT TerminalConnection m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader TerminalConnection)

runTerminalT :: (MonadIO m, MonadReader r m, Has (TVar Broker.Connections) r) => Inputs.TerminalUI -> TerminalT m a -> m (Either String a)
runTerminalT i (TerminalT t) = do
  eitherConn <- fmap TerminalConnection <$> Broker.getClientById (Inputs.stream_id i)
  case eitherConn of
    Right conn -> Right <$> runReaderT t conn
    Left err -> pure $ Left err

instance (MonadIO m) => MonadTerminal (TerminalT m) where
  output r = do
    c <- asks client
    unwrapReply =<< liftIO (gRpcCall @'MsgProtoBuf @TerminalUIService @"TerminalUIService" @"Output" c $ OutputRequest r)
  isInteractive = do
    c <- asks client
    fmap interactive $
      unwrapReply =<< liftIO (gRpcCall @'MsgProtoBuf @TerminalUIService @"TerminalUIService" @"IsInteractive" c)
  events = do
    c <- asks client
    (eventCon, respCon) <- liftIO (gRpcCall @'MsgProtoBuf @TerminalUIService @"TerminalUIService" @"Events" c Compressed)
    pure
      ( C.transPipe liftIO eventCon,
        C.transPipe liftIO $ void respCon -- TODO: Don't ignore the reply
      )

-- TODO: Remove requirement of Show
-- TODO: Handle errors better
unwrapReply :: (MonadIO m, Show a) => GRpcReply a -> m a
unwrapReply reply =
  case reply of
    GRpcOk a -> pure a
    err -> error $ show err
