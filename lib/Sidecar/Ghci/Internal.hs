{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Sidecar.Ghci.Internal where

import Data.String.Interpolate (i)
import Optics ((^.))

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Optics.TH
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO
import qualified System.Process as Process
import qualified System.Random as Random


data Ghci s = Ghci
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  , processHandle :: Process.ProcessHandle
  , commandIORef :: IORef Text
  , separatorIORef :: IORef Text
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''Ghci


withGhci :: forall m a. MonadIO m => Text -> (forall s. Ghci s -> IO a) -> m a
withGhci command action = liftIO do
  Process.withCreateProcess processConfig setup

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

        commandIORef <- newIORef ""
        separatorIORef <- newIORef ""

        let ghci = Ghci
              { stdinHandle
              , stdoutHandle
              , stderrHandle
              , processHandle
              , commandIORef
              , separatorIORef
              }

        Text.IO.hPutStrLn stdinHandle [i|:set prompt ""|]
        Text.IO.hPutStrLn stdinHandle [i|:set prompt-cont ""|]
        run ghci "import qualified System.IO as SIDECAR"

        action ghci

      _ ->
        fail "Failed to create GHCi handles"


run :: Ghci s -> Text -> IO ()
run ghci command = do
  send ghci command
  ignore ghci


query :: Ghci s -> Text -> IO (Text, Text)
query ghci command = do
  send ghci command
  receive ghci


send :: Ghci s -> Text -> IO ()
send ghci command = do
  random <- Random.randomRIO @Int (0, 1_000_000)
  let separator = [i|__sidecar__#{random}__|]

  atomicWriteIORef (ghci ^. #separatorIORef) separator
  atomicWriteIORef (ghci ^. #commandIORef) command

  Text.IO.hPutStrLn (ghci ^. #stdinHandle) command
  Text.IO.hPutStrLn (ghci ^. #stdinHandle)
    [i|SIDECAR.hPutStrLn SIDECAR.stdout "\\n#{separator}"|]
  Text.IO.hPutStrLn (ghci ^. #stdinHandle)
    [i|SIDECAR.hPutStrLn SIDECAR.stderr "\\n#{separator}"|]


receive :: Ghci s -> IO (Text, Text)
receive ghci = do
  separator <- readIORef (ghci ^. #separatorIORef)
  command <- readIORef (ghci ^. #commandIORef)

  let stream handle =
        Streamly.repeatM (Text.IO.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.filter (/= command)
          & Streamly.filter (not . Text.null)
          & Streamly.map (`Text.snoc` '\n')
          & Streamly.foldl' (<>) mempty
          & fmap Text.stripEnd

  Async.concurrently
    do stream (ghci ^. #stdoutHandle)
    do stream (ghci ^. #stderrHandle)


ignore :: Ghci s -> IO ()
ignore ghci = do
  separator <- readIORef (ghci ^. #separatorIORef)

  let stream handle =
        Streamly.repeatM (Text.IO.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.drain

  Async.concurrently_
    do stream (ghci ^. #stdoutHandle)
    do stream (ghci ^. #stderrHandle)
