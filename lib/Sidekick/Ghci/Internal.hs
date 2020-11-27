{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Interact with a live GHCi session

module Sidekick.Ghci.Internal
  (
    Ghci (..)
  -- * Start GHCi session
  , withGhci
  -- * High-level operations
  -- | High-level wrappers for 'send', 'receive', and 'receive_'. Calls to 'send'
  -- are always followed by 'receive' or 'receive_' to ensure the GHCi session
  -- is in a good state for the next command.
  , run
  , run_
  , cancel
  -- * Low-level operations
  -- | Low-level primitives for more direct manipulation of the GHCi session,
  -- providing no checks or guarantees that you maintain a good state.
  , send
  , receive
  , receive_
  -- * Debugging
  , interact
  )
where

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


-- | GHCi session state
data Ghci s = Ghci
  { stdinHandle :: Handle
  -- ^ Handle for GHCi session's @stdin@ stream
  , stdoutHandle :: Handle
  -- ^ Handle for GHCi session's @stderr@ stream
  , stderrHandle :: Handle
  -- ^ Handle for GHCi session's @stdin@ stream
  , processHandle :: Process.ProcessHandle
  -- ^ Process handle for GHCi session
  , commandIORef :: IORef Text
  -- ^ Mutable reference to last executed command
  , separatorIORef :: IORef Text
  -- ^ Mutable reference to last separator
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''Ghci


-- | Run operations on a live GHCi session
--
-- >>> withGhci "ghci" $ \ghci -> run ghci ":type fmap"
-- ("fmap :: Functor f => (a -> b) -> f a -> f b","")
withGhci
  :: forall m a
   . MonadIO m
  => Text
  -- ^ Command to start GHCi session (e.g. @ghci@ or @cabal repl@)
  -> (forall s. Ghci s -> IO a)
  -- ^ Operations using the GHCi session
  -> m a
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

        -- Disable prompt
        Text.IO.hPutStrLn stdinHandle [i|:set prompt ""|]
        Text.IO.hPutStrLn stdinHandle [i|:set prompt-cont ""|]

        -- Import 'System.IO' for 'hPutStrLn' and friends, and print initial
        -- separator
        run_ ghci "import qualified System.IO as SIDEKICK"

        action ghci

      _ ->
        fail "Failed to create GHCi handles"


-- | Run a command in GHCi, collecting its output 'System.IO'
run
  :: Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> IO (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
run ghci command = do
  send ghci command
  receive ghci


-- | Run a command in GHCi, ignoring its output
run_
  :: Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> IO ()
run_ ghci command = do
  send ghci command
  receive_ ghci


-- | Send Ctrl-C (@SIGINT@) to GHCi session. Useful for interrupting
-- long-running commands.
cancel
  :: Ghci s
  -- ^ GHCi session handle
  -> IO ()
cancel ghci =
  Process.interruptProcessGroupOf (ghci ^. #processHandle)


-- | Run a command in GHCi
send
  :: Ghci s
  -- ^ GHCi session state
  -> Text
  -- ^ GHCi command or Haskell expression
  -> IO ()
send ghci command = do
  random <- Random.randomRIO @Int (0, 1_000_000)
  let separator = [i|__sidekick__#{random}__|]

  atomicWriteIORef (ghci ^. #separatorIORef) separator
  atomicWriteIORef (ghci ^. #commandIORef) command

  Text.IO.hPutStrLn (ghci ^. #stdinHandle) command
  Text.IO.hPutStrLn (ghci ^. #stdinHandle)
    [i|SIDEKICK.hPutStrLn SIDEKICK.stdout "\\n#{separator}"|]
  Text.IO.hPutStrLn (ghci ^. #stdinHandle)
    [i|SIDEKICK.hPutStrLn SIDEKICK.stderr "\\n#{separator}"|]


-- | Collect output from the previously run command
receive
  :: Ghci s
  -- ^ GHCi session state
  -> IO (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
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


-- | Ignore output from the previously run command
receive_
  :: Ghci s
  -- ^ GHCi session state
  -> IO ()
receive_ ghci = do
  separator <- readIORef (ghci ^. #separatorIORef)

  let stream handle =
        Streamly.repeatM (Text.IO.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.drain

  Async.concurrently_
    do stream (ghci ^. #stdoutHandle)
    do stream (ghci ^. #stderrHandle)


-- | Interact with the GHCi session directly via @stdin@ and @stdout@. Useful
-- for debugging and experimenting.
--
-- >>> withGhci "ghci" interact
-- 1 + 1
-- 2
interact
  :: Ghci s
  -- ^ GHCi session state
  -> IO ()
interact ghci = do
  Streamly.repeatM getLine
    & Streamly.mapM (run ghci)
    & Streamly.trace (\(out, err) -> do
        unless (Text.null err) $ Text.IO.hPutStrLn stderr err
        unless (Text.null out) $ putTextLn out
      )
    & Streamly.drain
