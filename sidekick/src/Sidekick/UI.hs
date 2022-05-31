module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Prelude hiding (State, state)

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified System.Console.ANSI as Ansi


data State = State
  { loading :: Bool
  , out :: Text
  , err :: Text
  }


data Event
  = Loading Bool
  | ModifyOut (Text -> Text)
  | ModifyErr (Text -> Text)


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
  { loading = True
  , out = ""
  , err = ""
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
draw State{loading, out = (Text.strip -> out), err} = one do
  Brick.vBox $ catMaybes
    [ Just $ Brick.txt content
        & Brick.padBottom Brick.Max
        & Brick.padRight Brick.Max

    , if out == mempty then Nothing else Just Brick.hBorder

    , Just $ Brick.txt out
        & Brick.vLimitPercent 50
        & Brick.padRight Brick.Max
    ]
  where
  colored color = toText $ Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull color]
  bold = toText $ Ansi.setSGRCode [Ansi.SetConsoleIntensity Ansi.BoldIntensity]
  reset = toText $ Ansi.setSGRCode [Ansi.Reset]
  content
    | loading =
        bold <> colored Ansi.Yellow <> "Loading..." <> reset <> err
    | Text.null err =
        bold <> colored Ansi.Green <> "All good" <> reset
    | otherwise =
        err


chooseCursor
  :: State
  -> [Brick.CursorLocation Name]
  -> Maybe (Brick.CursorLocation Name)
chooseCursor _state _cursorLocations = Nothing


handleEvent
  :: State
  -> Brick.BrickEvent Name Event
  -> Brick.EventM Name (Brick.Next State)
handleEvent state@State{out, err} = \case
  Brick.AppEvent appEvent -> case appEvent of
    Loading loading ->
      Brick.continue state{loading}

    ModifyOut f ->
      Brick.continue state{out = f out}

    ModifyErr f ->
      Brick.continue state{err = f err}

  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey Vty.KEsc [] ->
      Brick.halt state

    Vty.EvKey (Vty.KChar 'q') [] ->
      Brick.halt state

    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] ->
      Brick.halt state

    _ ->
      Brick.continue state

  _ ->
    Brick.continue state


startEvent :: State -> Brick.EventM Name State
startEvent = pure


attrMap :: State -> Brick.AttrMap
attrMap _state = Brick.attrMap Vty.defAttr []
