module Sidekick (main) where

import Sidekick.Ghci (Ghci)
import Sidekick.Options (Options (..))

import qualified Brick.BChan as Brick
import qualified Data.Text as Text
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified Sidekick.Watch as Watch
import qualified Streamly.Prelude as Streamly
import qualified UnliftIO.Async as Async


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions

  uiEventChannel <- Brick.newBChan 10

  let reload :: Ghci s -> IO ()
      reload ghci = do
        Brick.writeBChan uiEventChannel (UI.Loading True)
        Ghci.send ghci ":reload"
        (outStream, errStream) <- Ghci.receiveStreaming ghci

        Async.concurrently_
          do
            Brick.writeBChan uiEventChannel (UI.ModifyOut \_ -> mempty)

            outStream
              & Streamly.trace (\line -> do
                  Brick.writeBChan uiEventChannel (UI.ModifyOut (<> ('\n' `Text.cons` line)))
                )
              & Streamly.drain

          do
            err <-
              errStream
                & Streamly.map (`Text.snoc` '\n')
                & Streamly.foldl' (<>) mempty
                & fmap Text.stripEnd

            Brick.writeBChan uiEventChannel (UI.ModifyErr \_ -> err)

        Brick.writeBChan uiEventChannel (UI.Loading False)

  Async.race_
    do
      UI.start uiEventChannel
    do
      Ghci.withGhci (fromMaybe "cabal repl" command) \ghci -> do
        reload ghci
        Watch.start directory \_ -> reload ghci
