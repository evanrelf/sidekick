{-# LANGUAGE FlexibleContexts #-}

module Sidekick.Watch (start) where

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly


start :: Streamly.MonadAsync m => Maybe FilePath -> m ()
start userDirectory = do
  let directory = fromMaybe "." userDirectory

  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.hasExtension "hs")
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)

  (_stopWatchingToken, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.trace handleEvent
    & Streamly.drain


handleEvent :: MonadIO m => Streamly.FSNotify.Event -> m ()
handleEvent = \case
  Streamly.FSNotify.Added _path _time Streamly.FSNotify.NotDir -> do
    pass

  Streamly.FSNotify.Modified _path _time Streamly.FSNotify.NotDir -> do
    pass

  Streamly.FSNotify.Removed _path _time Streamly.FSNotify.NotDir -> do
    pass

  _ ->
    pass
