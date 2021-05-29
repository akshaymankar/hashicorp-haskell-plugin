{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit ((.|))
import qualified Conduit as C
import Control.Monad.IO.Class
import Hashicorp.Plugin (PluginT)
import Waypoint.Build (BuildImpl (..), BuildResp (..))
import Waypoint.Inputs (Input (..), InputType (..), Inputs (..))
import Waypoint.Plugin
import qualified Waypoint.TerminalUI as Terminal
import Waypoint.TerminalUI.Types

main :: IO ()
main = startDummyServer

startDummyServer :: IO ()
startDummyServer = servePlugin dummyBuilder

dummyBuilder :: BuildImpl PluginT '[ 'InputTypeSource, 'InputTypeJobInfo, 'InputTypeApp, 'InputTypeTerminalUI, 'InputTypeLabelSet, 'InputTypeLogger]
dummyBuilder = BuildImpl $
  \(InputSource src :* InputJobInfo ji :* InputApp app :* InputTerminalUI ui :* InputLabelSet ls :* InputLogger l :* Nil) -> do
    liftIO $ print (src, ji, app, ui, ls, l)
    _ <- Terminal.runTerminalT ui $ do
      Terminal.output ["a line!!", "and another one!"]
      (eventConduit, _respConduit) <- Terminal.events
      C.runConduit $
        ( do
            C.yield . EventLine $ Line "header line" headerStyle
            C.yield . EventLine $ Line "error line" errorStyle
            C.yield . EventLine $ Line "error bold line" errorBoldStyle
            C.yield . EventLine $ Line "warning  line" warningStyle
            C.yield . EventLine $ Line "warning bold line" warningBoldStyle
            C.yield . EventLine $ Line "info line" infoStyle
            C.yield . EventLine $ Line "success line" successStyle
            C.yield . EventLine $ Line "success bold line" successBoldStyle
            C.yield . EventLine $ Line "arbitrary line" "arbitrary"
            C.yield . EventInput $ Input "Input please" "" False
            C.yield . EventInput $ Input "Secret please" "" True
            -- Yielding this makes waypoint segfault!
            -- C.yield . EventTable $
            --   Table
            --     ["header1", "header2"]
            --     [ TableRow [TableEntry "row1-entry1" "", TableEntry "row1-entry2" ""],
            --       TableRow [TableEntry "row2-entry1" "", TableEntry "row2-entry2" ""]
            --     ]
        )
          .| eventConduit
    pure $ BuildResp Nothing mempty mempty
