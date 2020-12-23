{-# LANGUAGE OverloadedLists #-}

module Sidekick (main) where

import Sidekick.Options (Options (..))

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Sidekick.FSNotify as FSNotify
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified UnliftIO.Async as Async


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions

  (_uiInChan, uiOutChan) <- Unagi.newChan
  -- (ghciInChan, ghciOutChan) <- Unagi.newChan
  -- (fsnotifyInChan, fsnotifyOutChan) <- Unagi.newChan

  racing_
    [ startGhci command
    , FSNotify.start directory
    , UI.start uiOutChan
    ]


startGhci :: Maybe Text -> IO ()
startGhci maybeCommand = do
  let command = fromMaybe "cabal repl" maybeCommand

  Ghci.withGhci command \_ghci -> do
    -- TODO
    pass


racing :: NonEmpty (IO a) -> IO a
racing = go [] . toList
  where
  go threads [] =
    snd <$> Async.waitAny threads
  go threads (action : actions) =
    Async.withAsync action \thread -> go (thread : threads) actions


racing_ :: NonEmpty (IO a) -> IO ()
racing_ = void . racing
