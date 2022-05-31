{-# LANGUAGE FlexibleContexts #-}

module Sidekick.Watch
  ( Event (..)
  , start
  )
where

import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly
import qualified System.FilePath as FilePath
import qualified Witch


data Event
  = CabalModified FilePath
  | HaskellAdded FilePath
  | HaskellModified FilePath
  | HaskellRemoved FilePath


start
  :: Streamly.MonadAsync m
  => Maybe FilePath
  -> (Streamly.FSNotify.StopWatching m -> Event -> m ())
  -> m ()
start userDirectory handleEvent = do
  let directory = fromMaybe "." userDirectory

  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)
          (Streamly.FSNotify.disj
            (Streamly.FSNotify.hasExtension "hs")
            (Streamly.FSNotify.hasExtension "cabal"))

  (stopWatching, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.mapMaybe (Witch.tryInto @Event >>> rightToMaybe)
    & Streamly.trace (handleEvent stopWatching)
    & Streamly.drain


instance Witch.TryFrom Streamly.FSNotify.Event Event where
  tryFrom = Witch.maybeTryFrom \case
    Streamly.FSNotify.Added path _time Streamly.FSNotify.NotDir ->
      Just (HaskellAdded path)

    Streamly.FSNotify.Modified path _time Streamly.FSNotify.NotDir
      | "cabal" `FilePath.isExtensionOf` path ->
          Just (CabalModified path)

      | otherwise ->
          Just (HaskellModified path)

    Streamly.FSNotify.Removed path _time Streamly.FSNotify.NotDir ->
      Just (HaskellRemoved path)

    _ ->
      Nothing
