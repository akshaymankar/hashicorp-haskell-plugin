{-# LANGUAGE FlexibleContexts #-}

module Waypoint.Plugin where

import qualified Data.HashMap.Lazy as HashMap
import Hashicorp.Plugin (Plugin (..))
import qualified Hashicorp.Plugin as Plugin
import Waypoint.Build (BuildImpl, builderService)
import Waypoint.Inputs (DecodeInputs, KnownInputTypes)
import Waypoint.Map (mapperService)
import Mu.GRpc.Server (MultipleServers(..))

servePlugin :: (DecodeInputs inputs, KnownInputTypes inputs) => BuildImpl Plugin.PluginT inputs -> IO ()
servePlugin builder =
  Plugin.serve
    ( Plugin.ServeConfig
        (Plugin.HandshakeConfig "WAYPOINT_PLUGIN" "be6c1928786a4df0222c13eef44ac846da2c0d461d99addc93f804601c6b7205")
        (HashMap.singleton 1 (Plugin $ MSOneMore (builderService builder) $ MSOneMore mapperService MSEnd))
    )
