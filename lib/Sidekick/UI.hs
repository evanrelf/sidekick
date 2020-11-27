{-# LANGUAGE TemplateHaskell #-}

module Sidekick.UI (main) where

import Optics ((^.))

import qualified Brick
import qualified Graphics.Vty as Vty
import qualified Optics.TH


data State = State
  { text :: Text
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''State


data Event = Event


type Name = ()


main :: IO ()
main = do
  _finalState <- Brick.defaultMain application initialState
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


handleEvent :: State -> Brick.BrickEvent n e -> Brick.EventM n (Brick.Next State)
handleEvent state = \case
  Brick.VtyEvent vtyEvent ->
    handleVtyEvent state vtyEvent

  Brick.AppEvent appEvent ->
    Brick.continue state

  Brick.MouseDown name button modifiers location ->
    Brick.continue state

  Brick.MouseUp name maybeButton location ->
    Brick.continue state


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
