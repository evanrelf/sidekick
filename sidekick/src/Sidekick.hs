{-# LANGUAGE OverloadedLists #-}

module Sidekick (main) where

import Sidekick.Options (Options (..))

import qualified Brick.BChan as Brick
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified Sidekick.Watch as Watch
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.MVar as MVar


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions

  mvar <- MVar.newMVar ()
  uiChan <- Brick.newBChan 10

  let startGhci = do
        Ghci.withGhci (fromMaybe "cabal repl" command) \ghci -> do
          -- Ghci.run_ ghci ":set -ddump-json"

          forever do
            _ <- MVar.takeMVar mvar
            (out, err) <- Ghci.run ghci ":reload"
            Brick.writeBChan uiChan (UI.NewText (out, err))

  racing_
    [ Watch.start mvar directory
    , UI.start uiChan
    , startGhci
    ]


racing :: NonEmpty (IO a) -> IO a
racing = go [] . toList
  where
  go threads [] =
    snd <$> Async.waitAny threads
  go threads (action : actions) =
    Async.withAsync action \thread -> go (thread : threads) actions


racing_ :: NonEmpty (IO a) -> IO ()
racing_ = void . racing
