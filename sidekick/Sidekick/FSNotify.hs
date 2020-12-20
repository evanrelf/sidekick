{-# LANGUAGE FlexibleContexts #-}

module Sidekick.FSNotify (start) where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Pub.Unagi (Pub, pub)

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly


start
  :: Streamly.MonadAsync m
  => Has (Lift IO) sig m
  => Has (Pub FilePath) sig m
  => FilePath
  -> m ()
start directory = do
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


handleEvent
  :: MonadIO m
  => Has (Lift IO) sig m
  => Has (Pub FilePath) sig m
  => Streamly.FSNotify.Event
  -> m ()
handleEvent = \case
  Streamly.FSNotify.Added path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Added " <> show path)
    pub path

  Streamly.FSNotify.Modified path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Modified " <> show path)
    pub path

  Streamly.FSNotify.Removed path _time Streamly.FSNotify.NotDir -> do
    putStrLn ("Removed " <> show path)
    pub path

  _ ->
    pass
