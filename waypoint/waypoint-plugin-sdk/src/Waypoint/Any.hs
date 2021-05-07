{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Waypoint.Any where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Waypoint.Proto (WaypointServerSchema)

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
