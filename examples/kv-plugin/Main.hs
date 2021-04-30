{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Control.Monad.State (MonadIO (..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Hashicorp.Plugin as Hashicorp
import Mu.GRpc.Server (MultipleServers (..))
import Mu.Quasi.GRpc (grpc)
import Mu.Schema (Term, (:/:))
import Mu.Schema.Optics (record, record1, (^.))
import Mu.Server (MonadServer, ServerT, method, singleService)

grpc "KVSchema" id "kv.proto"

main :: IO ()
main =
  Hashicorp.serve $
    Hashicorp.ServeConfig
      (Hashicorp.HandshakeConfig "BASIC_PLUGIN" "hello")
      (Map.singleton 1 (Hashicorp.Plugin $ MSOneMore kvServer MSEnd))

type TermFrom ty field = Term ty (ty :/: field)

type GetRequest = TermFrom KVSchema "GetRequest"

type GetResponse = TermFrom KVSchema "GetResponse"

type PutRequest = TermFrom KVSchema "PutRequest"

type Empty = TermFrom KVSchema "Empty"

getKV :: (MonadServer m, MonadIO m) => GetRequest -> m GetResponse
getKV req =
  liftIO $
    record1
      <$> BS.readFile (filename $ req ^. #key)

putKV :: (MonadServer m, MonadIO m) => PutRequest -> m Empty
putKV req = do
  fmap record . liftIO $
    BS.writeFile
      (filename $ req ^. #key)
      (req ^. #value)

filename :: Text.Text -> FilePath
filename = Text.unpack . ("kv_" <>)

kvServer :: (MonadServer m, MonadIO m) => ServerT '[] i KV m _
kvServer =
  singleService
    ( method @"Get" getKV,
      method @"Put" putKV
    )
