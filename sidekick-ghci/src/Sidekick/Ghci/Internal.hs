{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interact with a live GHCi session

module Sidekick.Ghci.Internal
  ( Ghci (..)

    -- * Start GHCi session
  , withGhci

    -- * High-level operations
    -- | High-level wrappers for 'send', 'receive', and 'receive_'. Calls to
    -- 'send' are always followed by 'receive' or 'receive_' to ensure the GHCi
    -- session is in a good state for the next command.
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

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Text (Text)
import UnliftIO (MonadUnliftIO)
import UnliftIO.IO (Handle)
import Prelude hiding (interact)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Streamly.Prelude as Streamly
import qualified System.Random as Random
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.IO as IO
import qualified UnliftIO.Process as Process
import qualified UnliftIO.STM as STM


-- | GHCi session handle
data Ghci s = Ghci
  { stdinHandle :: Handle
  -- ^ Handle for GHCi session's @stdin@ stream

  , stdoutHandle :: Handle
  -- ^ Handle for GHCi session's @stderr@ stream

  , stderrHandle :: Handle
  -- ^ Handle for GHCi session's @stdin@ stream

  , processHandle :: Process.ProcessHandle
  -- ^ Process handle for GHCi session

  , commandTVar :: STM.TVar Text
  -- ^ Mutable reference to last executed command

  , separatorTVar :: STM.TVar Text
  -- ^ Mutable reference to last separator
  }


-- | Run operations on a live GHCi session
--
-- >>> withGhci "ghci" $ \ghci -> run ghci ":type fmap"
-- ("fmap :: Functor f => (a -> b) -> f a -> f b","")
withGhci
  :: forall m a
   . MonadUnliftIO m
  => Text
  -- ^ Command to start GHCi session (e.g. @ghci@ or @cabal repl@)
  -> (forall s. Ghci s -> m a)
  -- ^ Operations using the GHCi session
  -> m a
withGhci command action = Process.withCreateProcess processConfig setup
  where
  processConfig :: Process.CreateProcess
  processConfig =
    (Process.shell (Text.unpack command))
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
    -> m a
  setup maybeStdinHandle maybeStdoutHandle maybeStderrHandle processHandle =
    case (maybeStdinHandle, maybeStdoutHandle, maybeStderrHandle) of
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
        commandTVar <- STM.newTVarIO ""
        separatorTVar <- STM.newTVarIO ""

        let ghci = Ghci
              { stdinHandle
              , stdoutHandle
              , stderrHandle
              , processHandle
              , commandTVar
              , separatorTVar
              }

        IO.hSetBuffering stdinHandle IO.LineBuffering
        IO.hSetBuffering stdoutHandle IO.LineBuffering
        IO.hSetBuffering stderrHandle IO.LineBuffering

        -- Disable prompt
        liftIO $ Text.hPutStrLn stdinHandle ":set prompt \"\""
        liftIO $ Text.hPutStrLn stdinHandle ":set prompt-cont \"\""

        -- Import 'System.IO' for 'hPutStrLn' and friends, and print initial
        -- separator
        run_ ghci "import qualified System.IO as SIDEKICK"

        -- Enable color
        run_ ghci ":set -fdiagnostics-color=always"

        -- Disable settings that produce ugly output
        run_ ghci ":unset +t +s"

        action ghci

      _ ->
        Exception.throwString "Failed to create GHCi handles"


-- | Run a command in GHCi, collecting its output
run
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
run ghci command = do
  send ghci command
  receive ghci


-- | Run a command in GHCi, ignoring its output
run_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m ()
run_ ghci command = do
  send ghci command
  receive_ ghci


-- | Send Ctrl-C (@SIGINT@) to GHCi session. Useful for interrupting
-- long-running commands.
cancel
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
cancel ghci = Process.interruptProcessGroupOf (processHandle ghci)


-- | Run a command in GHCi
send
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m ()
send ghci command = do
  random <- liftIO $ Random.randomRIO @Int (0, 1_000_000)

  let separator :: Text
      separator = Text.pack ("__sidekick__" <> show random <> "__")

  STM.atomically do
    STM.writeTVar (separatorTVar ghci) separator
    STM.writeTVar (commandTVar ghci) command

  liftIO $ Text.hPutStrLn (stdinHandle ghci) command
  liftIO $ Text.hPutStrLn (stdinHandle ghci)
    ("SIDEKICK.hPutStrLn SIDEKICK.stdout \"\\n" <> separator <> "\"")
  liftIO $ Text.hPutStrLn (stdinHandle ghci)
    ("SIDEKICK.hPutStrLn SIDEKICK.stderr \"\\n" <> separator <> "\"")


-- | Collect output from the previously run command
receive
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
receive ghci = do
  (separator, command) <- STM.atomically do
    separator <- STM.readTVar (separatorTVar ghci)
    command <- STM.readTVar (commandTVar ghci)
    pure (separator, command)

  let stream :: Handle -> IO Text
      stream handle =
        Streamly.repeatM (Text.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.filter (/= command)
          & Streamly.map (`Text.snoc` '\n')
          & Streamly.foldl' (<>) mempty
          & fmap Text.stripEnd

  liftIO $ Async.concurrently
    do stream (stdoutHandle ghci)
    do stream (stderrHandle ghci)


-- | Ignore output from the previously run command
receive_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
receive_ ghci = do
  separator <- STM.atomically $ STM.readTVar (separatorTVar ghci)

  let stream :: Handle -> IO ()
      stream handle =
        Streamly.repeatM (Text.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.drain

  liftIO $ Async.concurrently_
    do stream (stdoutHandle ghci)
    do stream (stderrHandle ghci)


-- | Interact with the GHCi session directly via @stdin@ and @stdout@. Useful
-- for debugging and experimenting.
--
-- >>> withGhci "ghci" interact
-- 1 + 1
-- 2
interact
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
interact ghci = liftIO do
  Streamly.repeatM Text.getLine
    & Streamly.mapM (run ghci)
    & Streamly.trace (\(out, err) -> do
        unless (Text.null err) $ Text.hPutStrLn IO.stderr err
        unless (Text.null out) $ Text.hPutStrLn IO.stdout out
      )
    & Streamly.drain
