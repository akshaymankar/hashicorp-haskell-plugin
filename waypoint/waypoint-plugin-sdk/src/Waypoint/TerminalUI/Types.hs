{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Waypoint.TerminalUI.Types where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP (I (..), NS (..))
import Mu.Schema (CustomFieldMapping (..), FromSchema, Mapping ((:->)), ToSchema)
import qualified Waypoint.GoogleProto as GoogleProto
import Waypoint.Proto (WaypointServerSchema)

newtype OutputRequest = OutputRequest {lines :: [Text]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.OutputRequest",
      FromSchema WaypointServerSchema "TerminalUI.OutputRequest"
    )

newtype IsInteractiveResponse = IsInteractiveRequest {interactive :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.IsInteractiveResponse",
      FromSchema WaypointServerSchema "TerminalUI.IsInteractiveResponse"
    )

newtype Event = Event {event :: NS I '[Line, Status, NamedValues, Raw, Table, StepGroup, Step, Input]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event",
      FromSchema WaypointServerSchema "TerminalUI.Event"
    )

pattern EventLine :: Line -> Event
pattern EventLine l = Event (Z (I l))

pattern EventStatus :: Status -> Event
pattern EventStatus s = Event (S (Z (I s)))

pattern EventNamedValues :: NamedValues -> Event
pattern EventNamedValues n = Event (S (S (Z (I n))))

pattern EventRaw :: Raw -> Event
pattern EventRaw r = Event (S (S (S (Z (I r)))))

pattern EventTable :: Table -> Event
pattern EventTable t = Event (S (S (S (S (Z (I t))))))

pattern EventStepGroup :: StepGroup -> Event
pattern EventStepGroup s = Event (S (S (S (S (S (Z (I s)))))))

pattern EventStep :: Step -> Event
pattern EventStep s = Event (S (S (S (S (S (S (Z (I s))))))))

pattern EventInput :: Input -> Event
pattern EventInput i = Event (S (S (S (S (S (S (S (Z (I i)))))))))

data Input = Input
  { prompt :: Text,
    style :: Style,
    secret :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Input",
      FromSchema WaypointServerSchema "TerminalUI.Event.Input"
    )

data InputResp = InputResp
  { input :: Text,
    error_ :: Maybe GoogleProto.Status
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( ToSchema WaypointServerSchema "TerminalUI.Event.InputResp",
      FromSchema WaypointServerSchema "TerminalUI.Event.InputResp"
    )
    via (CustomFieldMapping "TerminalUI.Event.InputResp" '["error_" ':-> "error"] InputResp)

data Status = Status
  { status :: Text,
    msg :: Text,
    step :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Status",
      FromSchema WaypointServerSchema "TerminalUI.Event.Status"
    )

type Style = Text

headerStyle, errorStyle, errorBoldStyle, warningStyle, warningBoldStyle, infoStyle, successStyle, successBoldStyle :: Style
headerStyle = "header"
errorStyle = "error"
errorBoldStyle = "error-bold"
warningStyle = "warning"
warningBoldStyle = "warning-bold"
infoStyle = "info"
successStyle = "success"
successBoldStyle = "success-bold"

data Line = Line {msg :: Text, style :: Style}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Line",
      FromSchema WaypointServerSchema "TerminalUI.Event.Line"
    )

data Raw = Raw {data_ :: ByteString, stderr :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Raw",
      FromSchema WaypointServerSchema "TerminalUI.Event.Raw"
    )
    via (CustomFieldMapping "TerminalUI.Event.Raw" '["data_" ':-> "data"] Raw)

data NamedValue = NamedValue {name :: Text, value :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.NamedValue",
      FromSchema WaypointServerSchema "TerminalUI.Event.NamedValue"
    )

newtype NamedValues = NamedValues {values :: [NamedValue]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.NamedValues",
      FromSchema WaypointServerSchema "TerminalUI.Event.NamedValues"
    )

data TableEntry = TableEntry {value :: Text, color :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.TableEntry",
      FromSchema WaypointServerSchema "TerminalUI.Event.TableEntry"
    )

newtype TableRow = TableRow {entries :: [TableEntry]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.TableRow",
      FromSchema WaypointServerSchema "TerminalUI.Event.TableRow"
    )

data Table = Table {headers :: [Text], rows :: [TableRow]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Table",
      FromSchema WaypointServerSchema "TerminalUI.Event.Table"
    )

newtype StepGroup = StepGroup {close :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Event.StepGroup",
      FromSchema WaypointServerSchema "TerminalUI.Event.StepGroup"
    )

data Step = Step
  { id_ :: Int32,
    close :: Bool,
    msg :: Text,
    status :: Text,
    output :: ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( ToSchema WaypointServerSchema "TerminalUI.Event.Step",
      FromSchema WaypointServerSchema "TerminalUI.Event.Step"
    )
    via (CustomFieldMapping "TerminalUI.Event.Step" '["id_" ':-> "id"] Step)

newtype Response = Response {event :: NS I '[InputResp]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass
    ( ToSchema WaypointServerSchema "TerminalUI.Response",
      FromSchema WaypointServerSchema "TerminalUI.Response"
    )

pattern ResponseInputResp :: InputResp -> Response
pattern ResponseInputResp i = Response (Z (I i))
