{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.IO.Class
import Hashicorp.Plugin (PluginT)
import Waypoint.Build (BuildImpl (..), BuildResp (..))
import Waypoint.Inputs (Input (..), InputType (..), Inputs (..))
import Waypoint.Plugin

main :: IO ()
main = startDummyServer

startDummyServer :: IO ()
startDummyServer = servePlugin dummyBuilder

dummyBuilder :: BuildImpl PluginT '[ 'InputTypeSource, 'InputTypeJobInfo, InputTypeApp, 'InputTypeTerminalUI, InputTypeLabelSet, InputTypeLogger]
dummyBuilder = BuildImpl $
  \(InputSource src :* InputJobInfo ji :* InputApp app :* InputTerminalUI ui :* InputLabelSet ls :* InputLogger log :* Nil) -> do
    liftIO $ print (src, ji, app, ui, ls, log)
    pure $ BuildResp Nothing mempty mempty
