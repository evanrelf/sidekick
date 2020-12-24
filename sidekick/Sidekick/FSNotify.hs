{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Sidekick.FSNotify (start) where

import Data.String.Interpolate (i)

import qualified Streamly
import qualified Streamly.FSNotify
import qualified Streamly.Prelude as Streamly
import qualified UnliftIO.MVar as MVar


start :: Streamly.MonadAsync m => MVar () -> Maybe FilePath -> m ()
start mvar maybeDirectory = do
  let directory = fromMaybe "." maybeDirectory

  -- We only care about filesystem events concerning Haskell files
  let eventPredicate =
        Streamly.FSNotify.conj
          (Streamly.FSNotify.hasExtension "hs")
          (Streamly.FSNotify.invert Streamly.FSNotify.isDirectory)

  (_stopWatchingToken, eventStream) <-
    Streamly.FSNotify.watchTree directory eventPredicate

  eventStream
    & Streamly.trace (handleEvent mvar)
    & Streamly.drain


handleEvent :: MonadIO m => MVar () -> Streamly.FSNotify.Event -> m ()
handleEvent mvar = \case
  Streamly.FSNotify.Added path time Streamly.FSNotify.NotDir -> do
    -- putStrLn [i|#{time}: Added #{path}|]
    _ <- MVar.tryPutMVar mvar ()
    pass

  Streamly.FSNotify.Modified path time Streamly.FSNotify.NotDir -> do
    -- putStrLn [i|#{time}: Modified #{path}|]
    _ <- MVar.tryPutMVar mvar ()
    pass

  Streamly.FSNotify.Removed path time Streamly.FSNotify.NotDir -> do
    -- putStrLn [i|#{time}: Removed #{path}|]
    _ <- MVar.tryPutMVar mvar ()
    pass

  _ ->
    pass
