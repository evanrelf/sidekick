{-# LANGUAGE NamedFieldPuns #-}

module Sidecar.Ghci
  ( GhciHandle
  , withGhci
  )
where

import qualified System.Process as Process


data GhciHandle = GhciHandle
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  , processHandle :: Process.ProcessHandle
  }


withGhci :: MonadIO m => Text -> (GhciHandle -> IO a) -> m a
withGhci command f = liftIO do
  let processConfig =
        (Process.shell (toString command))
          { Process.std_in = Process.CreatePipe
          , Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          -- TODO @evan: What are process groups?
          -- , Process.create_group = True
          }

  Process.withCreateProcess processConfig \maybeStdinHandle maybeStdoutHandle maybeStderrHandle processHandle -> do
    case (maybeStdinHandle, maybeStdoutHandle, maybeStderrHandle) of
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle) ->
        f GhciHandle{stdinHandle, stdoutHandle, stderrHandle, processHandle}
      _ ->
        fail "Failed to create GHCi handles"
