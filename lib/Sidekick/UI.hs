{-# LANGUAGE TemplateHaskell #-}

module Sidekick.UI (main) where

import Optics ((.~), (^.))

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Graphics.Vty as Vty
import qualified Optics.TH


data State = State
  { text :: Text
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''State


data Event
  = NewText Text


type Name = ()


main :: Brick.BChan Event -> IO ()
main eventChannel = do
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
  { text = "Hello world"
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
draw state =
  [ Brick.txt (state ^. #text)
  ]


chooseCursor
  :: State
  -> [Brick.CursorLocation n]
  -> Maybe (Brick.CursorLocation n)
chooseCursor state cursorLocations = Nothing


handleEvent
  :: State
  -> Brick.BrickEvent n Event
  -> Brick.EventM n (Brick.Next State)
handleEvent state = \case
  Brick.VtyEvent vtyEvent ->
    handleVtyEvent state vtyEvent

  Brick.AppEvent appEvent ->
    handleAppEvent state appEvent

  Brick.MouseDown name button modifiers location ->
    Brick.continue state

  Brick.MouseUp name maybeButton location ->
    Brick.continue state


handleAppEvent :: State -> Event -> Brick.EventM n (Brick.Next State)
handleAppEvent state = \case
  NewText text ->
    Brick.continue (state & #text .~ text)


handleVtyEvent :: State -> Vty.Event -> Brick.EventM n (Brick.Next State)
handleVtyEvent state = \case
  -- Quit
  Vty.EvKey key [] | key `elem` [Vty.KEsc, Vty.KChar 'q'] ->
    Brick.halt state

  Vty.EvKey key modifiers ->
    Brick.continue state

  Vty.EvMouseDown column row button modifiers ->
    Brick.continue state

  Vty.EvMouseUp column row maybeButton ->
    Brick.continue state

  Vty.EvResize width height ->
    Brick.continue state

  Vty.EvPaste bytes ->
    Brick.continue state

  Vty.EvLostFocus ->
    Brick.continue state

  Vty.EvGainedFocus ->
    Brick.continue state


startEvent :: State -> Brick.EventM n State
startEvent state = pure state


attrMap :: State -> Brick.AttrMap
attrMap state = Brick.attrMap Vty.defAttr []
