{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, liftWith, sendIO)
import Control.Carrier.Sub.Unagi (Sub, sub)
import Optics ((.~), (^.))

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Control.Concurrent.Async as Async
import qualified Graphics.Vty as Vty
import qualified Optics.TH


newtype State = State
  { text :: Text
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''State


newtype Event
  = NewText Text


type Name = ()


unagiToBChan
  :: Has (Lift IO) sig m
  => Has (Sub msg) sig m
  => Brick.BChan msg
  -> m ()
unagiToBChan bChan = forever do
  msg <- sub
  sendIO $ Brick.writeBChan bChan msg


-- Warning: Do not use this with stateful effects!
--
-- Any threads that do not return (either because they lose the race, or they
-- fail to finish due to an exception) will have their functorial state
-- discarded.
race_ :: Has (Lift IO) sig m => m a -> m b -> m ()
race_ action1 action2 =
  liftWith \hdl ctx -> either id id <$>
    Async.race
      do void <$> hdl (action1 <$ ctx)
      do void <$> hdl (action2 <$ ctx)


start
  :: Has (Lift IO) sig m
  => Has (Sub Event) sig m
  => m ()
start = do
  let buildVtyHandle = Vty.mkVty Vty.defaultConfig
  vtyHandle <- sendIO buildVtyHandle

  eventChannel <- sendIO $ Brick.newBChan 10

  let brick = void $ sendIO $
        Brick.customMain
          vtyHandle
          buildVtyHandle
          (Just eventChannel)
          application
          initialState

  race_
    do unagiToBChan eventChannel
    do brick


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
