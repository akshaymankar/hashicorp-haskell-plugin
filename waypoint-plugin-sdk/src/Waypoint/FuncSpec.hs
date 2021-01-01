{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Waypoint.FuncSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, Mapping ((:->)), ToSchema)
import Mu.Schema.Class (CustomFieldMapping (..))
import Waypoint.Any
import Waypoint.Proto (WaypointServerSchema)

type FuncSpecValueMapping =
  [ "typ" ':-> "type",
    "foo" ':-> "bar" -- TODO: Derivation doesn't work without this!
  ]

data FuncSpecValue = FuncSpecValue
  { name :: Text,
    typ :: Text,
    value :: Maybe Any
  }
  deriving (Show, Eq, Generic)
  deriving
    (ToSchema WaypointServerSchema "FuncSpec.Value", FromSchema WaypointServerSchema "FuncSpec.Value")
    via (CustomFieldMapping "FuncSpec.Value" FuncSpecValueMapping FuncSpecValue)

newtype FuncSpecArgs = FuncSpecArgs {args :: [FuncSpecValue]}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "FuncSpec.Args",
      FromSchema WaypointServerSchema "FuncSpec.Args"
    )

data FuncSpec = FuncSpec
  { name :: Text,
    args :: [FuncSpecValue],
    result :: [FuncSpecValue]
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "FuncSpec",
      FromSchema WaypointServerSchema "FuncSpec"
    )
