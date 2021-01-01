{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Waypoint.Proto where

import Mu.Quasi.GRpc (grpc)

grpc "WaypointServerSchema" id "waypoint-server.proto"
