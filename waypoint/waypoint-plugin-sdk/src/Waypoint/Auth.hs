{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Waypoint.Auth where

import GHC.Generics (Generic)
import Mu.Schema (FromSchema, ToSchema)
import Waypoint.FuncSpec (FuncSpec, FuncSpecArgs)
import Waypoint.Misc (ImplementsResp (ImplementsResp))
import Waypoint.Proto (WaypointServerSchema)

newtype AuthResponse = AuthResponse {authenticated :: Bool}
  deriving
    ( Show,
      Eq,
      Generic,
      ToSchema WaypointServerSchema "Auth.AuthResponse",
      FromSchema WaypointServerSchema "Auth.AuthResponse"
    )

-- TODO: Implement Authentication
isAuthenticator :: Monad m => m ImplementsResp
isAuthenticator = pure $ ImplementsResp False

auth :: Monad m => FuncSpecArgs -> m AuthResponse
auth _ = pure $ AuthResponse True

authSpec :: Monad m => m FuncSpec
authSpec = undefined

validateAuth :: Monad m => FuncSpecArgs -> m ()
validateAuth _ = pure ()

validateAuthSpec :: Monad m => m FuncSpec
validateAuthSpec = undefined
