module Sidekick.FSNotify (start) where

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly


start :: FilePath -> IO ()
start directory = do
  -- We only care about filesystem events concerning Haskell files
  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.hasExtension "hs")
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)

  (stopWatchingToken, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.trace handleEvent
    & Streamly.drain


handleEvent :: Streamly.FSNotify.Event -> IO ()
handleEvent = \case
  Streamly.FSNotify.Added path time Streamly.FSNotify.NotDir ->
    pass

  Streamly.FSNotify.Modified path time Streamly.FSNotify.NotDir ->
    pass

  Streamly.FSNotify.Removed path time Streamly.FSNotify.NotDir ->
    pass

  _ ->
    pass
