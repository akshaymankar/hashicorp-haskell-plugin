{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Waypoint.Map where

import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Mu.Server (MonadServer, SingleServerT)
import qualified Mu.Server as Mu
import Waypoint.Any (Any)
import Waypoint.FuncSpec (FuncSpec, FuncSpecArgs)
import Waypoint.Proto (Mapper, WaypointServerSchema)

data MapRequest = MapRequest
  { args :: Maybe FuncSpecArgs,
    result :: Text
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Map.Request",
      FromSchema WaypointServerSchema "Map.Request"
    )

newtype MapResponse = MapResponse {result :: Maybe Any}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Map.Response",
      FromSchema WaypointServerSchema "Map.Response"
    )

newtype MapListResponse = MapListResponse {funcs :: [FuncSpec]}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Map.ListResponse",
      FromSchema WaypointServerSchema "Map.ListResponse"
    )

listMappers :: Monad m => m MapListResponse
listMappers = pure $ MapListResponse mempty

-- TODO: Implement this
mapperMap :: Monad m => MapRequest -> m MapResponse
mapperMap = undefined

mapperService :: MonadServer m => SingleServerT info Mapper m _
mapperService =
  Mu.singleService
    ( Mu.method @"ListMappers" listMappers,
      Mu.method @"Map" mapperMap
    )
