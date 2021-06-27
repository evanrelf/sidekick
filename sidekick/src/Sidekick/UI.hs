module Sidekick.UI
  ( Event (..)
  , start
  )
where

import Optics (over, set, view)
import Relude.Extra (universe)
import Sidekick.Ghci.Parsers (unescape)
import Prelude hiding (State, state)

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Brick.Markup as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified System.Console.ANSI as Ansi

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


type Parser = Megaparsec.Parsec Void Text


data State = State
  { loading :: Bool
  , out :: Text
  , err :: Text
  } deriving stock Generic


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
draw state = one do
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
  out = Text.strip (view #out state)
  err = toText . unescape . toString $ view #err state
  colored color = toText $ Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull color]
  bold = toText $ Ansi.setSGRCode [Ansi.SetConsoleIntensity Ansi.BoldIntensity]
  reset = toText $ Ansi.setSGRCode [Ansi.Reset]
  content
    | view #loading state =
        bold <> colored Ansi.Yellow <> "Loading..." <> reset <> err
    | err == mempty =
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
  Loading loading ->
    Brick.continue (set #loading loading state)

  ModifyOut f ->
    Brick.continue do
      state
        & over #out f

  ModifyErr f ->
    Brick.continue do
      state
        & over #err f


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
attrMap _state = Brick.attrMap Vty.defAttr
  [ ( "ghci" <> "error"
    , Vty.defAttr `Vty.withForeColor` Vty.red
    )
  , ( "ghci" <> "warning"
    , Vty.defAttr `Vty.withForeColor` Vty.magenta
    )
  , ( "ghci" <> "line-number"
    , Vty.defAttr `Vty.withForeColor` Vty.blue
    )
  ]


-- ansiToMarkup :: Text -> Brick.Markup Brick.AttrName
-- ansiToMarkup = fst . Text.foldl' cons nil
--   where
--   nil = (mempty, Nothing)

--   cons (markup, Nothing) char = undefined
--   cons (markup, Just escape) 'm' = undefined


ansiToMarkup :: Text -> Brick.Markup Brick.AttrName
ansiToMarkup = either undefined id . Megaparsec.parse parser ""
  where
  parser = do
    text1 <- Megaparsec.takeWhileP Nothing (/= '\\')
    (layer, intensity, color, text2) <- parseColored undefined
    pure undefined

  parseColored = Megaparsec.between parseAnsiColor parseAnsiReset


parseAnsiColor :: Parser (Ansi.ConsoleLayer, Ansi.ColorIntensity, Ansi.Color)
parseAnsiColor = asum do
  (layer, intensity, color, code) <- do
    layer <- universe @Ansi.ConsoleLayer
    intensity <- universe @Ansi.ColorIntensity
    color <- universe @Ansi.Color
    let code = toText (Ansi.setSGRCode [Ansi.SetColor layer intensity color])
    pure (layer, intensity, color, code)
  pure $ (layer, intensity, color) <$ Megaparsec.try (Megaparsec.string code)


parseAnsiReset :: Parser ()
parseAnsiReset = do
  let reset =  toText (Ansi.setSGRCode [Ansi.Reset])
  () <$ Megaparsec.try (Megaparsec.string reset)
