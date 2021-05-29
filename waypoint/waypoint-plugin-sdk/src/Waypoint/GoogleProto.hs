{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Waypoint.GoogleProto where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Waypoint.Proto (WaypointServerSchema)

data Status = Status
  { code :: Int32,
    message :: Text,
    details :: [Any]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "google.rpc.Status",
      FromSchema WaypointServerSchema "google.rpc.Status"
    )

data Any = Any
  { type_url :: Text,
    value :: ByteString
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "google.protobuf.Any",
      FromSchema WaypointServerSchema "google.protobuf.Any"
    )
