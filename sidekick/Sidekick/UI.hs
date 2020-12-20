{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Optics ((.~), (^.))

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Graphics.Vty as Vty
import qualified Optics.TH


newtype State = State
  { text :: Text
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''State


newtype Event
  = NewText Text


type Name = ()


unagiToBChan :: MonadIO m => Unagi.OutChan msg -> Brick.BChan msg -> m ()
unagiToBChan outChan bChan = liftIO $ forever do
  msg <- Unagi.readChan outChan
  Brick.writeBChan bChan msg


start :: MonadIO m => Unagi.OutChan Event -> m ()
start outChan = liftIO do
  let buildVtyHandle = Vty.mkVty Vty.defaultConfig

  vtyHandle <- buildVtyHandle

  eventChannel <- Brick.newBChan 10

  Async.race_
    do
      unagiToBChan outChan eventChannel
    do
      Brick.customMain
        vtyHandle
        buildVtyHandle
        (Just eventChannel)
        application
        initialState


initialState :: State
initialState = State
  { text = "Loading..."
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
  NewText newText ->
    Brick.continue (state & #text .~ newText)


handleVtyEvent :: State -> Vty.Event -> Brick.EventM n (Brick.Next State)
handleVtyEvent state = \case
  -- Quit
  Vty.EvKey key [] | key `elem` [Vty.KEsc, Vty.KChar 'q'] ->
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


startEvent :: State -> Brick.EventM n State
startEvent = pure


attrMap :: State -> Brick.AttrMap
attrMap _state = Brick.attrMap Vty.defAttr []
