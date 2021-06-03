module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Optics (set, view)
import Prelude hiding (State, state)

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Graphics.Vty as Vty


data State = State
  { out :: Text
  , err :: Text
  } deriving stock Generic


data Event
  = NewText (Text, Text)


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
  { out = "Loading stdout..."
  , err = "Loading stderr..."
  }


application :: Brick.App State Event Name
application = Brick.App
  { Brick.appDraw = draw
  , Brick.appChooseCursor = chooseCursor
  , Brick.appHandleEvent = handleEvent
  , Brick.appStartEvent = startEvent
  , Brick.appAttrMap = attrMap
  }


draw :: State -> [Brick.Widget n]
draw state = one do
  Brick.vBox
    [ Brick.txt ("OUT: " <> view #out state)
        & Brick.vLimitPercent 50
        & Brick.padBottom Brick.Max
        & Brick.padRight Brick.Max
        -- & Brick.borderWithLabel (Brick.txt "stdout")
        -- & Brick.withBorderStyle (Brick.borderStyleFromChar ' ')

    , Brick.txt ("ERR: " <> view #err state)
        & Brick.vLimitPercent 50
        & Brick.padBottom Brick.Max
        & Brick.padRight Brick.Max
        -- & Brick.borderWithLabel (Brick.txt "stderr")
        -- & Brick.withBorderStyle (Brick.borderStyleFromChar ' ')
    ]


chooseCursor
  :: State
  -> [Brick.CursorLocation n]
  -> Maybe (Brick.CursorLocation n)
chooseCursor _state _cursorLocations = Nothing


handleEvent
  :: State
  -> Brick.BrickEvent n Event
  -> Brick.EventM n (Brick.Next State)
handleEvent state = \case
  Brick.VtyEvent vtyEvent ->
    handleVtyEvent state vtyEvent

  Brick.AppEvent appEvent ->
    handleAppEvent state appEvent

  Brick.MouseDown _name _button _modifiers _location ->
    Brick.continue state

  Brick.MouseUp _name _maybeButton _location ->
    Brick.continue state


handleAppEvent :: State -> Event -> Brick.EventM n (Brick.Next State)
handleAppEvent state = \case
  NewText (out, err) ->
    Brick.continue do
      state
        & set #out out
        & set #err err


handleVtyEvent :: State -> Vty.Event -> Brick.EventM n (Brick.Next State)
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


startEvent :: State -> Brick.EventM n State
startEvent = pure


attrMap :: State -> Brick.AttrMap
attrMap _state = Brick.attrMap Vty.defAttr []
