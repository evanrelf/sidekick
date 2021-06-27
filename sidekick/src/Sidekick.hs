module Sidekick (main) where

import Sidekick.Ghci (Ghci)
import Sidekick.Options (Options (..))

import qualified Brick.BChan as Brick
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Ghci.Json as Json
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI
import qualified Sidekick.Watch as Watch
import qualified Streamly.Prelude as Streamly
import qualified UnliftIO.Async as Async


main :: IO ()
main = newMain


newMain :: IO ()
newMain = do
  Options{command, directory} <- Options.getOptions

  uiEventChannel <- Brick.newBChan 10

  Async.race_
    do
      UI.start uiEventChannel
    do
      Ghci.withGhci (fromMaybe "cabal repl --repl-options=-fno-code" command) \ghci -> do
        Ghci.run_ ghci ":set -ddump-json"
        reload ghci uiEventChannel
        Watch.start directory \_ -> reload ghci uiEventChannel


reload :: Ghci s -> Brick.BChan UI.Event -> IO ()
reload ghci uiEventChannel = do
  Ghci.send ghci ":reload"
  (outStream, errStream) <- Ghci.receiveStreaming ghci

  Brick.writeBChan uiEventChannel (UI.ModifyMessages \_ -> mempty)

  outStream
    & Streamly.map (\line ->
        (line, Aeson.eitherDecodeStrict @Json.Message (encodeUtf8 line)))
    & Streamly.trace (\case
        (line, Left err) ->
          pure ()
        (_, Right message) ->
          Brick.writeBChan uiEventChannel (UI.ModifyMessages (`Vector.snoc` message))
      )
    & Streamly.drain


-- main :: IO ()
-- main = const newMain do
--   Options{command, directory} <- Options.getOptions

--   uiEventChannel <- Brick.newBChan 10

--   let reload :: Ghci s -> IO ()
--       reload ghci = do
--         Brick.writeBChan uiEventChannel (UI.Loading True)
--         Ghci.send ghci ":reload"
--         (outStream, errStream) <- Ghci.receiveStreaming ghci

--         Async.concurrently_
--           do
--             Brick.writeBChan uiEventChannel (UI.ModifyOut \_ -> mempty)

--             outStream
--               & Streamly.trace (\line -> do
--                   Brick.writeBChan uiEventChannel (UI.ModifyOut (<> ('\n' `Text.cons` line)))
--                 )
--               & Streamly.drain

--           do
--             err <-
--               errStream
--                 & Streamly.map (`Text.snoc` '\n')
--                 & Streamly.foldl' (<>) mempty
--                 & fmap Text.stripEnd

--             Brick.writeBChan uiEventChannel (UI.ModifyErr \_ -> err)

--         Brick.writeBChan uiEventChannel (UI.Loading False)

--   Async.race_
--     do
--       UI.start uiEventChannel
--     do
--       Ghci.withGhci (fromMaybe "cabal repl --repl-options=-fno-code" command) \ghci -> do
--         reload ghci
--         Watch.start directory \_ -> reload ghci
