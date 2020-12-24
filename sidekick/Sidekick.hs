module Sidekick (main) where

import Sidekick.Options (Options (..))

import qualified Brick.BChan as Brick
import qualified Sidekick.FSNotify as FSNotify
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.MVar as MVar


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions

  mvar <- MVar.newMVar ()
  uiChan <- Brick.newBChan 10

  Async.withAsync (FSNotify.start mvar directory) \_fsnotifyThread ->
    Async.withAsync (UI.start uiChan) \_uiThread -> do
      -- Ghci.withGhci (fromMaybe "cabal repl" command) \ghci -> do
        Concurrent.threadDelay 1_000_000

        forever do
          _ <- MVar.takeMVar mvar
          -- (out, _err) <- Ghci.run ghci ":reload"
          let out = "testing"
          Brick.writeBChan uiChan (UI.NewText out)


-- racing :: NonEmpty (IO a) -> IO a
-- racing = go [] . toList
--   where
--   go threads [] =
--     snd <$> Async.waitAny threads
--   go threads (action : actions) =
--     Async.withAsync action \thread -> go (thread : threads) actions


-- racing_ :: NonEmpty (IO a) -> IO ()
-- racing_ = void . racing
