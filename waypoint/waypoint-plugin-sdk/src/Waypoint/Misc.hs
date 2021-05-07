{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Waypoint.Misc where

import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Waypoint.Proto (WaypointServerSchema)

newtype ImplementsResp = ImplementsResp {implements :: Bool}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "ImplementsResp",
      FromSchema WaypointServerSchema "ImplementsResp"
    )
