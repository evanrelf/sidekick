{-# OPTIONS_GHC -Wno-orphans #-}

module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Data.Vector (Vector)
import Optics
import Prettyprinter (Pretty (..))
import Sidekick.Ghci.Json
import Prelude hiding (State, state)

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.List as Brick
import qualified Data.Text.Prettyprint.Doc.Render.Vty as Pretty.Vty
import qualified Graphics.Vty as Vty
import qualified Prettyprinter as Pretty


data State = State
  { messages :: Vector Message
  } deriving stock Generic


data Event
  = ModifyMessages (Vector Message -> Vector Message)


type Name = ()


start :: MonadIO m => Brick.BChan Event -> m ()
start eventChannel = liftIO do
  let buildVtyHandle = Vty.mkVty Vty.defaultConfig

  vtyHandle <- buildVtyHandle

  _finalState <-
    Brick.customMain
      vtyHandle
      buildVtyHandle
      (Just eventChannel)
      application
      initialState

  pass


initialState :: State
initialState = State
  { messages = mempty
  }


application :: Brick.App State Event Name
application = Brick.App
  { Brick.appDraw = draw
  , Brick.appChooseCursor = chooseCursor
  , Brick.appHandleEvent = handleEvent
  , Brick.appStartEvent = startEvent
  , Brick.appAttrMap = attrMap
  }


draw :: State -> [Brick.Widget Name]
draw state = one do
  Brick.renderList
    -- Render function
    (\_isSelected message ->
      prettyMessage message
        & Pretty.Vty.render
        & Brick.raw
        & Brick.padRight Brick.Max
        & Brick.padRight Brick.Max
        & Brick.border
    )
    -- Does list have focus?
    True
    -- List
    (Brick.list
      -- List name
      ()
      -- List elements
      (view #messages state)
      -- Minimum list element height
      1)


chooseCursor
  :: State
  -> [Brick.CursorLocation Name]
  -> Maybe (Brick.CursorLocation Name)
chooseCursor _state _cursorLocations = Nothing


handleEvent
  :: State
  -> Brick.BrickEvent Name Event
  -> Brick.EventM Name (Brick.Next State)
handleEvent state = \case
  Brick.VtyEvent vtyEvent ->
    handleVtyEvent state vtyEvent

  Brick.AppEvent appEvent ->
    handleAppEvent state appEvent

  Brick.MouseDown _name _button _modifiers _location ->
    Brick.continue state

  Brick.MouseUp _name _maybeButton _location ->
    Brick.continue state


handleAppEvent :: State -> Event -> Brick.EventM Name (Brick.Next State)
handleAppEvent state = \case
  ModifyMessages f ->
    Brick.continue (state & over #messages f)


handleVtyEvent :: State -> Vty.Event -> Brick.EventM Name (Brick.Next State)
handleVtyEvent state = \case
  -- Quit
  Vty.EvKey key modifiers | isQuit key modifiers ->
    Brick.halt state

  Vty.EvKey _key _modifiers ->
    Brick.continue state

  Vty.EvMouseDown _column _row _button _modifiers ->
    Brick.continue state

  Vty.EvMouseUp _column _row _maybeButton ->
    Brick.continue state

  Vty.EvResize _width _height ->
    Brick.continue state

  Vty.EvPaste _bytes ->
    Brick.continue state

  Vty.EvLostFocus ->
    Brick.continue state

  Vty.EvGainedFocus ->
    Brick.continue state

  where
  isQuit key modifiers = or
    [ key `elem` [Vty.KEsc, Vty.KChar 'q']
    , Vty.MCtrl `elem` modifiers && key == Vty.KChar 'c'
    ]


startEvent :: State -> Brick.EventM Name State
startEvent = pure


attrMap :: State -> Brick.AttrMap
attrMap _state = Brick.attrMap Vty.defAttr []


prettyMessage :: Message -> Pretty.Doc Vty.Attr
prettyMessage message =
  Pretty.vsep $ fmap (fromMaybe mempty) $ filter isJust
    [ liftA2 (<>) span severity
    , Just $ Pretty.indent 4 doc
    -- , reason
    ]
  where
    span = (<> ": ") . prettySpan <$> view #span message
    severity = prettySeverity <$> view #severity message
    doc = pretty $ view #doc message
    reason = pretty <$> view #reason message


prettySpan :: Span -> Pretty.Doc ann
prettySpan Span{file, startLine, startCol, endLine, endCol}
  | startLine == endLine && startCol == endCol = mconcat
      [ pretty file
      , ":"
      , pretty startLine
      , ":"
      , pretty startCol
      ]
  | startLine == endLine = mconcat
      [ pretty file
      , ":"
      , pretty startLine
      , ":"
      , pretty startCol <> "-" <> pretty (endCol - 1)
      ]
  | otherwise = mconcat
      [ pretty file <> ":"
      , "(" <> pretty startLine <> "," <> pretty startCol <> ")"
      , "-"
      , "(" <> pretty endLine <> "," <> pretty endCol <> ")"
      ]


prettySeverity :: Severity -> Pretty.Doc Vty.Attr
prettySeverity = \case
  SevOutput -> mempty
  SevWarning -> Pretty.annotate magenta "warning: "
  SevError -> Pretty.annotate red "error: "
  SevFatal -> Pretty.annotate red "fatal: "
  where
  red = Vty.defAttr `Vty.withForeColor` Vty.red
  magenta = Vty.defAttr `Vty.withForeColor` Vty.magenta
