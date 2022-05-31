{-# LANGUAGE FlexibleContexts #-}

module Sidekick.Watch
  ( Event (..)
  , start
  )
where

import Prelude hiding (and, or)

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

  let eventPredicate = do
        let and = Streamly.FSNotify.conj
        let or = Streamly.FSNotify.disj

        let isFile = Streamly.FSNotify.invert Streamly.FSNotify.isDirectory
        let isHaskell = Streamly.FSNotify.hasExtension "hs"
        let isCabal = Streamly.FSNotify.hasExtension "cabal"

        isFile `and` (isHaskell `or` isCabal)

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
