{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sidecar.Ghci
  ( GhciM
  , withGhci
  , exit
  )
where

import Relude.Monad (MonadReader, ReaderT, ask, usingReaderT)

import qualified System.IO as IO
import qualified System.Process as Process


newtype GhciM a = GhciM { runGhciM :: ReaderT GhciHandle IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GhciHandle
    )


data GhciHandle = GhciHandle
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  , processHandle :: Process.ProcessHandle
  }


withGhci :: forall m a. MonadIO m => Text -> GhciM a -> m a
withGhci command action = liftIO $ Process.withCreateProcess processConfig setup
  where
  processConfig :: Process.CreateProcess
  processConfig =
    (Process.shell (toString command))
      { Process.std_in = Process.CreatePipe
      , Process.std_out = Process.CreatePipe
      , Process.std_err = Process.CreatePipe
      , Process.create_group = True
      }

  setup
    :: Maybe Handle
    -> Maybe Handle
    -> Maybe Handle
    -> Process.ProcessHandle
    -> IO a
  setup maybeStdinHandle maybeStdoutHandle maybeStderrHandle processHandle =
    case (maybeStdinHandle, maybeStdoutHandle, maybeStderrHandle) of
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
        IO.hSetBuffering stdinHandle IO.LineBuffering
        IO.hSetBuffering stdoutHandle IO.LineBuffering
        IO.hSetBuffering stderrHandle IO.LineBuffering

        let ghciHandle = GhciHandle
              { stdinHandle
              , stdoutHandle
              , stderrHandle
              , processHandle
              }

        usingReaderT ghciHandle . runGhciM $ action

      _ ->
        fail "Failed to create GHCi handles"


exit :: GhciM ()
exit = do
  GhciHandle{stdinHandle, stdoutHandle, stderrHandle, processHandle} <- ask
  liftIO $ Process.cleanupProcess
    ( Just stdinHandle
    , Just stdoutHandle
    , Just stderrHandle
    , processHandle
    )
