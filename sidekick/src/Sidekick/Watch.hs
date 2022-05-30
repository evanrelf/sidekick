{-# LANGUAGE FlexibleContexts #-}

module Sidekick.Watch
  ( Event (..)
  , start
  )
where

import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly
import qualified Witch


data Event
  = Added FilePath
  | Modified FilePath
  | Removed FilePath


start
  :: Streamly.MonadAsync m
  => Maybe FilePath
  -> (Streamly.FSNotify.StopWatching m -> Event -> m ())
  -> m ()
start userDirectory handleEvent = do
  let directory = fromMaybe "." userDirectory

  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.hasExtension "hs")
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)

  (stopWatching, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.mapMaybe (Witch.tryInto @Event >>> rightToMaybe)
    & Streamly.trace (handleEvent stopWatching)
    & Streamly.drain


instance Witch.TryFrom Streamly.FSNotify.Event Event where
  tryFrom = Witch.maybeTryFrom \case
    Streamly.FSNotify.Added path _time Streamly.FSNotify.NotDir ->
      Just (Added path)

    Streamly.FSNotify.Modified path _time Streamly.FSNotify.NotDir ->
      Just (Modified path)

    Streamly.FSNotify.Removed path _time Streamly.FSNotify.NotDir ->
      Just (Removed path)

    _ ->
      Nothing
