module Sidekick (main) where

import Sidekick.Ghci (Ghci)
import Sidekick.Options (Options (..))

import qualified Brick.BChan as Brick
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified Sidekick.Watch as Watch
import qualified UnliftIO.Async as Async


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions

  uiEventChannel <- Brick.newBChan 10

  let reload :: Ghci s -> IO ()
      reload ghci = do
        Brick.writeBChan uiEventChannel UI.Loading
        (out, err) <- Ghci.run ghci ":reload"
        Brick.writeBChan uiEventChannel (UI.NewText (out, err))

  Async.race_
    do
      UI.start uiEventChannel
    do
      Ghci.withGhci (fromMaybe "cabal repl" command) \ghci -> do
        reload ghci
        Watch.start directory \_ -> reload ghci
