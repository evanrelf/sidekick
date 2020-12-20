{-# LANGUAGE FlexibleContexts #-}

module Sidekick.FSNotify (start) where

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly


start :: Streamly.MonadAsync m => Maybe FilePath -> m ()
start maybeDirectory = do
  let directory = fromMaybe "." maybeDirectory

  -- We only care about filesystem events concerning Haskell files
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
  Streamly.FSNotify.Added path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Added " <> show path)

  Streamly.FSNotify.Modified path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Modified " <> show path)

  Streamly.FSNotify.Removed path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Removed " <> show path)

  _ ->
    pass
