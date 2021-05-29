{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Control.Concurrent.STM (TVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Hashicorp.GRpc.Broker as Broker
import qualified Hashicorp.Plugin as Plugin
import Mu.GRpc.Client.TyApps (GRpcReply (..), GrpcClient, gRpcCall)
import Mu.GRpc.Server (GRpcMessageProtocol (MsgProtoBuf), WrappedServer (..))
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (Term, (:/:))
import Mu.Schema.Optics (record, record1, (^.))
import Mu.Server (MonadServer, ServerError (..), ServerErrorCode (Invalid), ServerT, method, serverError, singleService)
import System.IO.Error (catchIOError)

grpc "KVSchema" id "bidirectional.proto"

main :: IO ()
main =
  Plugin.serve $
    Plugin.ServeConfig
      (Plugin.HandshakeConfig "BASIC_PLUGIN" "hello")
      (Map.singleton 1 (Plugin.Plugin [Srv kvServer]))

type TermFrom ty field = Term ty (ty :/: field)

type GetRequest = TermFrom KVSchema "GetRequest"

type GetResponse = TermFrom KVSchema "GetResponse"

type PutRequest = TermFrom KVSchema "PutRequest"

type SumRequest = TermFrom KVSchema "SumRequest"

type SumResponse = TermFrom KVSchema "SumResponse"

type Empty = TermFrom KVSchema "Empty"

getKV :: (MonadServer m, MonadIO m) => GetRequest -> m GetResponse
getKV req =
  liftIO $
    record1
      <$> (readIO =<< readFile (filename $ req ^. #key))

putKV :: (MonadServer m, MonadIO m, MonadReader r m, Has (TVar Broker.Connections) r) => PutRequest -> m Empty
putKV req = do
  let f = filename $ req ^. #key
  stored <- liftIO $ flip catchIOError (const $ pure 0) $ readIO =<< readFile f
  client <- either (serverError . ServerError Invalid) pure =<< Broker.getClientById (req ^. #add_server)
  sumResponse <- callSum client (record (stored, req ^. #value))
  case sumResponse of
    GRpcOk sum' -> fmap record . liftIO $ writeFile f (show (sum' ^. #r))
    err -> serverError $ ServerError Invalid $ show err

callSum :: (MonadIO m) => GrpcClient -> SumRequest -> m (GRpcReply SumResponse)
callSum c r = liftIO $ gRpcCall @'MsgProtoBuf @AddHelper @"AddHelper" @"Sum" c r

filename :: Text.Text -> FilePath
filename = Text.unpack . ("kv_" <>)

kvServer :: (MonadServer m, MonadIO m, MonadReader r m, Has (TVar Broker.Connections) r) => ServerT '[] i Counter m _
kvServer =
  singleService
    ( method @"Get" getKV,
      method @"Put" putKV
    )
