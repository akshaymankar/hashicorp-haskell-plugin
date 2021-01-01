{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Waypoint.Config where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, Mapping ((:->)), ToSchema)
import Mu.Schema.Class (CustomFieldMapping (CustomFieldMapping))
import Waypoint.Proto (WaypointServerSchema)
import Waypoint.ProtoStruct (ProtoStruct)

newtype ConfigStructResp = ConfigStructResp {struct :: Maybe ProtoStruct}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Config.StructResp",
      FromSchema WaypointServerSchema "Config.StructResp"
    )

newtype ConfigureRequest = ConfigureRequest {json :: ByteString}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Config.ConfigureRequest",
      FromSchema WaypointServerSchema "Config.ConfigureRequest"
    )

type FDMapping =
  [ "type_" ':-> "type",
    "default_" ':-> "default"
  ]

data FieldDocumentation = FieldDocumentation
  { name :: Text,
    synopsis :: Text,
    summary :: Text,
    optional :: Bool,
    env_var :: Text,
    type_ :: Text,
    default_ :: Text
  }
  deriving (Show, Eq, Generic)
  deriving
    (ToSchema WaypointServerSchema "Config.FieldDocumentation", FromSchema WaypointServerSchema "Config.FieldDocumentation")
    via (CustomFieldMapping "Config.FieldDocumentation" FDMapping FieldDocumentation)

data MapperDocumentation = MapperDocumentation
  { input :: Text,
    output :: Text,
    description :: Text
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Config.MapperDocumentation",
      FromSchema WaypointServerSchema "Config.MapperDocumentation"
    )

data Documentation = Documentation
  { description :: Text,
    example :: Text,
    input :: Text,
    output :: Text,
    fields :: Map Text FieldDocumentation,
    mappers :: [MapperDocumentation]
  }
  deriving (Show, Eq, Generic, ToSchema WaypointServerSchema "Config.Documentation")

defaultDoc :: Documentation
defaultDoc = Documentation "" "" "" "" mempty mempty

-- TODO: Implement Configuration
configStruct :: Monad m => m ConfigStructResp
configStruct = pure $ ConfigStructResp Nothing

configure :: Monad m => ConfigureRequest -> m ()
configure _ = pure ()

-- TODO: Implement Documentation
documentation :: Monad m => m Documentation
documentation = pure defaultDoc
