{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Waypoint.Inputs where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.List as List
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.TypeLits (symbolVal)
import Mu.Adapter.ProtoBuf (IsProtoSchema, parseProtoViaSchema)
import Mu.Schema (FromSchema)
import Mu.Schema.Class (ToSchema)
import Waypoint.Any (Any (..))
import Waypoint.FuncSpec
  ( FuncSpec (FuncSpec, args),
    FuncSpecArgs (FuncSpecArgs),
    FuncSpecValue (FuncSpecValue, typ, value),
  )
import Waypoint.Proto (WaypointServerSchema)

-- Not included:
-- context.Context (Maybe this is not required?)
-- These don't work:
-- InputTypeProject
-- InputTypeDeploymentConfig
-- InputTypeComponent
-- TODO: Figure out why these don't work
data InputType
  = InputTypeSource
  | InputTypeJobInfo
  | InputTypeApp
  | InputTypeTerminalUI
  | InputTypeLabelSet
  | InputTypeLogger

data Input (t :: InputType) where
  InputSource :: Source -> Input 'InputTypeSource
  InputJobInfo :: JobInfo -> Input 'InputTypeJobInfo
  InputApp :: App -> Input 'InputTypeApp
  InputTerminalUI :: TerminalUI -> Input 'InputTypeTerminalUI
  InputLabelSet :: LabelSet -> Input 'InputTypeLabelSet
  InputLogger :: Logger -> Input 'InputTypeLogger

data Source = Source {app :: Text, path :: Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema SourceProtobufName,
      FromSchema WaypointServerSchema SourceProtobufName
    )

data JobInfo = JobInfo {local :: Bool, workspace :: Text, id :: Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema JobInfoProtobufName,
      FromSchema WaypointServerSchema JobInfoProtobufName
    )

data App = App {cache_dir :: Text, data_dir :: Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema AppProtobufName,
      FromSchema WaypointServerSchema AppProtobufName
    )

-- TODO: Hide this and expose a nice interface
newtype TerminalUI = TerminalUI {stream_id :: Word32}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema UIProtobufName,
      FromSchema WaypointServerSchema UIProtobufName
    )

newtype LabelSet = LabelSet {labels :: Map Text Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema LabelSetProtobufName,
      FromSchema WaypointServerSchema LabelSetProtobufName
    )

-- TODO: Hide this and expose a nice logger interface
newtype Logger = Logger {name :: Text}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema LoggerProtobufName,
      FromSchema WaypointServerSchema LoggerProtobufName
    )

-- TODO: Maybe there is a generalized thing for this
data Inputs ts where
  Nil :: Inputs '[]
  (:*) :: Input t -> Inputs ts -> Inputs (t ': ts)

infixr 5 :*

class KnownInputType (i :: InputType) where
  inputTypeVal :: Proxy i -> InputType

instance KnownInputType 'InputTypeSource where inputTypeVal _ = InputTypeSource

instance KnownInputType 'InputTypeJobInfo where inputTypeVal _ = InputTypeJobInfo

instance KnownInputType 'InputTypeApp where inputTypeVal _ = InputTypeApp

instance KnownInputType 'InputTypeTerminalUI where inputTypeVal _ = InputTypeTerminalUI

instance KnownInputType 'InputTypeLabelSet where inputTypeVal _ = InputTypeLabelSet

instance KnownInputType 'InputTypeLogger where inputTypeVal _ = InputTypeLogger

-- | Get input types for a give type. This is used to create 'FuncSpec'.
class ToInputTypes a where
  toInputTypes :: Proxy a -> [InputType]

class KnownInputTypes (is :: [InputType]) where
  inputTypeVals :: Proxy is -> [InputType]

instance KnownInputTypes '[] where
  inputTypeVals _ = []

instance (KnownInputType t, KnownInputTypes ts) => KnownInputTypes (t ': ts) where
  inputTypeVals _ = inputTypeVal (Proxy @t) : inputTypeVals (Proxy @ts)

-- | Decode 'Input' from given 'FuncSpecArgs' for a given type
class DecodeInput t where
  decodeInput :: Proxy t -> FuncSpecArgs -> Either String (Input t)

instance DecodeInput 'InputTypeSource where
  decodeInput _ =
    fmap InputSource . parseFuncSpecArg sourceProtobufType

instance DecodeInput 'InputTypeJobInfo where
  decodeInput _ =
    fmap InputJobInfo . parseFuncSpecArg jobInfoProtobufType

instance DecodeInput 'InputTypeApp where
  decodeInput _ =
    fmap InputApp . parseFuncSpecArg appProtobufType

instance DecodeInput 'InputTypeTerminalUI where
  decodeInput _ =
    fmap InputTerminalUI . parseFuncSpecArg uiProtobufType

instance DecodeInput 'InputTypeLabelSet where
  decodeInput _ =
    fmap InputLabelSet . parseFuncSpecArg labelSetProtobufType

instance DecodeInput 'InputTypeLogger where
  decodeInput _ =
    fmap InputLogger . parseFuncSpecArg loggerProtobufType

parseFuncSpecArg :: forall a sty. (IsProtoSchema WaypointServerSchema sty, FromSchema WaypointServerSchema sty a) => Text -> FuncSpecArgs -> Either String a
parseFuncSpecArg protobufTypeName (FuncSpecArgs vals) = do
  val <- case (value :: FuncSpecValue -> Maybe Any) =<< List.find (\v -> typ v == protobufTypeName) vals of
    Nothing -> Left "The waypoint server did not return Source"
    Just v -> Right v
  first (\e -> "failed to parse protobuf: " <> show e) $
    parseProtoViaSchema @WaypointServerSchema (value (val :: Any))

class DecodeInputs ts where
  decodeInputs :: Proxy ts -> FuncSpecArgs -> Either String (Inputs ts)

instance DecodeInputs '[] where
  decodeInputs _ _ = pure Nil

instance (DecodeInput t, DecodeInputs ts) => DecodeInputs (t ': ts) where
  decodeInputs _ vals =
    (:*)
      <$> decodeInput (Proxy @t) vals
      <*> decodeInputs (Proxy @ts) vals

inputTypesToFuncSpec :: [InputType] -> FuncSpec
inputTypesToFuncSpec =
  foldr
    -- TODO: This is ugly, use optics/lens
    (\i fs -> (fs :: FuncSpec) {args = inputTypeToFuncSpecArg i : args (fs :: FuncSpec)})
    (FuncSpec "" [] [])

inputTypeToFuncSpecArg :: InputType -> FuncSpecValue
inputTypeToFuncSpecArg = \case
  InputTypeSource -> FuncSpecValue "" sourceProtobufType Nothing
  InputTypeJobInfo -> FuncSpecValue "" jobInfoProtobufType Nothing
  InputTypeApp -> FuncSpecValue "" appProtobufType Nothing
  InputTypeTerminalUI -> FuncSpecValue "" uiProtobufType Nothing
  InputTypeLabelSet -> FuncSpecValue "" labelSetProtobufType Nothing
  InputTypeLogger -> FuncSpecValue "" loggerProtobufType Nothing

-- * Constants

sdkProtobufPackage :: Text
sdkProtobufPackage = "hashicorp.waypoint.sdk"

type SourceProtobufName = "Args.Source"

sourceProtobufType :: Text
sourceProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @SourceProtobufName))

type JobInfoProtobufName = "Args.JobInfo"

jobInfoProtobufType :: Text
jobInfoProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @JobInfoProtobufName))

type AppProtobufName = "Args.DataDir.App"

appProtobufType :: Text
appProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @AppProtobufName))

type UIProtobufName = "Args.TerminalUI"

uiProtobufType :: Text
uiProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @UIProtobufName))

type LabelSetProtobufName = "Args.LabelSet"

labelSetProtobufType :: Text
labelSetProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @LabelSetProtobufName))

type LoggerProtobufName = "Args.Logger"

loggerProtobufType :: Text
loggerProtobufType = sdkProtobufPackage <> "." <> Text.pack (symbolVal (Proxy @LoggerProtobufName))
