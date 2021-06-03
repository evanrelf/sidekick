{-# LANGUAGE FlexibleContexts #-}

module Sidekick.Watch
  ( Event (..)
  , start
  )
where

import UnliftIO (MonadUnliftIO)

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly


data Event
  = Added FilePath
  | Modified FilePath
  | Removed FilePath


start
  :: MonadUnliftIO m
  => Streamly.MonadAsync m
  => Maybe FilePath
  -> (Event -> m ())
  -> m ()
start userDirectory handleEvent = do
  let directory = fromMaybe "." userDirectory

  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.hasExtension "hs")
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)

  (_stopWatchingToken, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.mapMaybe convertEvent
    & Streamly.trace handleEvent
    & Streamly.drain


convertEvent :: Streamly.FSNotify.Event -> Maybe Event
convertEvent = \case
  Streamly.FSNotify.Added path _time Streamly.FSNotify.NotDir ->
    Just (Added path)

  Streamly.FSNotify.Modified path _time Streamly.FSNotify.NotDir ->
    Just (Modified path)

  Streamly.FSNotify.Removed path _time Streamly.FSNotify.NotDir ->
    Just (Removed path)

  _ ->
    Nothing
