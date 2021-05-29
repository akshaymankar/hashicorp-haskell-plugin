{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Waypoint.Build where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Mu.Server (HandlersT (H0, (:<|>:)), MonadServer, SingleServerT)
import qualified Mu.Server as Mu
import System.IO (hPrint, stderr)
import Waypoint.GoogleProto (Any)
import Waypoint.Auth (auth, authSpec, isAuthenticator, validateAuth, validateAuthSpec)
import Waypoint.Config (configStruct, configure, documentation)
import Waypoint.FuncSpec (FuncSpec (..), FuncSpecArgs (..))
import Waypoint.Inputs (DecodeInputs (decodeInputs), InputType, Inputs, KnownInputTypes (inputTypeVals), inputTypesToFuncSpec)
import Waypoint.Proto (Builder, WaypointServerSchema)

data BuildResp = BuildResp
  { result :: Maybe Any,
    labels :: Map Text Text,
    template_data :: ByteString
  }
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Build.Resp",
      FromSchema WaypointServerSchema "Build.Resp"
    )

newtype BuildImpl m (is :: [InputType]) = BuildImpl {buildImpl :: Inputs is -> m BuildResp}

buildSpec :: forall inputs m. (KnownInputTypes inputs, Monad m) => BuildImpl m inputs -> m FuncSpec
buildSpec _ = pure (inputTypesToFuncSpec (inputTypeVals (Proxy @inputs)))

build :: forall m inputs. (MonadIO m, DecodeInputs inputs) => BuildImpl m inputs -> FuncSpecArgs -> m BuildResp
build builder funcArgs = do
  liftIO $ hPrint stderr funcArgs
  case decodeInputs (Proxy @inputs) funcArgs of
    Left err -> error $ "failed to parse inputs: " <> show err
    Right is -> buildImpl builder is

builderService :: (MonadServer m, DecodeInputs inputs, KnownInputTypes inputs) => BuildImpl m inputs -> SingleServerT info Builder m _
builderService builder =
  -- 'Mu.Server.singleService' cannot be used here because it does not work for
  -- more than 10 methods. This is because 'ToNamedList' is only defined for max
  -- 9 elements in a tuple.
  Mu.Server
    ( isAuthenticator
        :<|>: auth
        :<|>: authSpec
        :<|>: validateAuth
        :<|>: validateAuthSpec
        :<|>: configStruct
        :<|>: configure
        :<|>: documentation
        :<|>: buildSpec builder
        :<|>: build builder
        :<|>: H0
    )
