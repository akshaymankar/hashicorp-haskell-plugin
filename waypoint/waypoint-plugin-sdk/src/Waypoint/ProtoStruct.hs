{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Waypoint.ProtoStruct where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Mu.Schema (FieldValue (FSchematic, FUnion), FromSchema (..), Mapping ((:->)), NP (Nil, (:*)), NS (S, Z), Term (TRecord), ToSchema (..))
import Mu.Schema.Class (CustomFieldMapping (CustomFieldMapping))
import Mu.Schema.Interpretation (Field (Field))
import Waypoint.Proto (WaypointServerSchema)

newtype ProtoStructPrimitive = ProtoStructPrimitive {kind :: Word32}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "protostructure.Primitive",
      FromSchema WaypointServerSchema "protostructure.Primitive"
    )

data ProtoStructContainer = ProtoStructContainer
  { kind :: Word32,
    elem :: Maybe ProtoStructType,
    key :: Maybe ProtoStructType,
    count :: Int32
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "protostructure.Container",
      FromSchema WaypointServerSchema "protostructure.Container"
    )

data ProtoStructType
  = ProtoStructTypePrimitive ProtoStructPrimitive
  | ProtoStructTypeContainer ProtoStructContainer
  | ProtoStructTypeStruct ProtoStruct
  deriving (Show, Eq, Generic)

instance ToSchema WaypointServerSchema "protostructure.Type" ProtoStructType where
  toSchema t =
    let choice = case t of
          ProtoStructTypePrimitive p -> Z $ FSchematic $ toSchema p
          ProtoStructTypeContainer c -> S $ Z $ FSchematic $ toSchema c
          ProtoStructTypeStruct s -> S $ S $ Z $ FSchematic $ toSchema s
     in TRecord (Field (FUnion choice) :* Nil)

instance FromSchema WaypointServerSchema "protostructure.Type" ProtoStructType where
  fromSchema (TRecord (Field (FUnion choice) :* Nil)) =
    case choice of
      Z (FSchematic p) -> ProtoStructTypePrimitive $ fromSchema p
      S (Z (FSchematic c)) -> ProtoStructTypeContainer $ fromSchema c
      S (S (Z (FSchematic s))) -> ProtoStructTypeStruct $ fromSchema s
      S (S (S _)) -> error "TODO why is this needed?"

type ProtoStructFieldMapping =
  [ "name" ':-> "Name",
    "pkgPath" ':-> "PkgPath",
    "tag" ':-> "Tag",
    "typ" ':-> "type"
  ]

data ProtoStructField = ProtoStructField
  { name :: Text,
    pkgPath :: Text,
    tag :: Text,
    typ :: Maybe ProtoStructType
  }
  deriving (Show, Eq, Generic)
  deriving
    ( ToSchema WaypointServerSchema "protostructure.Struct.Field",
      FromSchema WaypointServerSchema "protostructure.Struct.Field"
    )
    via (CustomFieldMapping "protostructure.Struct.Field" ProtoStructFieldMapping ProtoStructField)

newtype ProtoStruct = ProtoStruct {fields :: [ProtoStructField]}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "protostructure.Struct",
      FromSchema WaypointServerSchema "protostructure.Struct"
    )
