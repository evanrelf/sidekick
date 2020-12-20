{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Sidekick.FSNotify (start) where

import Data.String.Interpolate (i)

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
  Streamly.FSNotify.Added path time Streamly.FSNotify.NotDir -> do
    putStrLn [i|#{time}: Added #{path}|]

  Streamly.FSNotify.Modified path time Streamly.FSNotify.NotDir -> do
    putStrLn [i|#{time}: Modified #{path}|]

  Streamly.FSNotify.Removed path time Streamly.FSNotify.NotDir -> do
    putStrLn [i|#{time}: Removed #{path}|]

  _ ->
    pass
