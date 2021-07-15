{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:     Sidekick.Ghci.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Sidekick.Ghci.Internal where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Function ((&))
import Data.Text (Text)
import System.IO (Handle)
import Prelude hiding (interact)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Streamly as Streamly
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO
import qualified System.Process as Process


-- | GHCi session handle.
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

  , promptNumberTVar :: STM.TVar Integer
    -- ^ Mutable reference to last prompt number

  , lockTMVar :: STM.TMVar ()
    -- ^ Lock to prevent concurrent access to GHCi session
  }


-- | Run operations on a live GHCi session.
--
-- >>> withGhci "ghci" $ \ghci -> run ghci ":type fmap"
-- ("fmap :: Functor f => (a -> b) -> f a -> f b","")
withGhci
  :: MonadUnliftIO m
  => Text
  -- ^ Command to start GHCi session (e.g. @ghci@ or @cabal repl@)
  -> (forall s. Ghci s -> m a)
  -- ^ Operations using the GHCi session
  -> m a
withGhci command action = withRunInIO \unliftIO ->
  Process.withCreateProcess processConfig \i o e p -> do
    ghci <- setup i o e p
    unliftIO $ action ghci
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
    -> IO (Ghci s)
  setup i o e processHandle =
    case (i, o, e) of
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
        commandTVar <- STM.newTVarIO ""
        promptNumberTVar <- STM.newTVarIO 0
        lockTMVar <- STM.newTMVarIO ()

        let ghci = Ghci
              { stdinHandle
              , stdoutHandle
              , stderrHandle
              , processHandle
              , commandTVar
              , promptNumberTVar
              , lockTMVar
              }

        IO.hSetBuffering stdinHandle IO.LineBuffering
        IO.hSetBuffering stdoutHandle IO.LineBuffering
        IO.hSetBuffering stderrHandle IO.LineBuffering

        -- Disable prompt
        Text.hPutStrLn stdinHandle ":set prompt \"\""
        Text.hPutStrLn stdinHandle ":set prompt-cont \"\""

        -- Import 'System.IO' for 'hPutStrLn' and friends, and print initial
        -- separator
        run_ ghci "import qualified System.IO as SIDEKICK"

        -- Enable color
        run_ ghci ":set -fdiagnostics-color=always"

        -- Disable settings that produce ugly output
        run_ ghci ":unset +t +s"

        pure ghci

      _ ->
        Exception.throwIO (userError "Failed to create GHCi handles")


-- | Run a command in GHCi, collecting its output.
run
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
run ghci command = liftIO $ withLock ghci do
  send ghci command
  receive ghci


-- | Run a command in GHCi, ignoring its output.
run_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m ()
run_ ghci command = liftIO $ withLock ghci do
  send ghci command
  receive_ ghci


-- | Run a command in GHCi.
send
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m ()
send ghci command = liftIO do
  n <- STM.atomically do
    promptNumber <- (+ 1) <$> STM.readTVar (promptNumberTVar ghci)

    STM.writeTVar (promptNumberTVar ghci) promptNumber
    STM.writeTVar (commandTVar ghci) command

    pure promptNumber

  liftIO do
    Text.hPutStrLn (stdinHandle ghci) command
    Text.hPutStrLn (stdinHandle ghci)
      ("SIDEKICK.hPutStrLn SIDEKICK.stdout \"\\n" <> separator n <> "\"")
    Text.hPutStrLn (stdinHandle ghci)
      ("SIDEKICK.hPutStrLn SIDEKICK.stderr \"\\n" <> separator n <> "\"")


-- | Stream output line-by-line from the previously run command.
receiveStreaming
  :: Streamly.MonadAsync m
  => Ghci s
  -- ^ GHCi session handle
  -> m (Streamly.SerialT m Text, Streamly.SerialT m Text)
  -- ^ @stdout@ and @stderr@ streams from GHCi
receiveStreaming ghci = liftIO do
  (n, command) <- STM.atomically do
    promptNumber <- STM.readTVar (promptNumberTVar ghci)
    command <- STM.readTVar (commandTVar ghci)
    pure (promptNumber, command)

  let stream :: Streamly.MonadAsync m => Handle -> Streamly.SerialT m Text
      stream handle =
        Streamly.repeatM (liftIO $ Text.hGetLine handle)
          & Streamly.takeWhile (/= separator n)
          & Streamly.filter (/= command)

  pure
    ( stream (stdoutHandle ghci)
    , stream (stderrHandle ghci)
    )


-- | Collect output from the previously run command.
receive
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m (Text, Text)
  -- ^ @stdout@ and @stderr@ from GHCi
receive ghci = liftIO do
  (stdoutStream, stderrStream) <- receiveStreaming ghci

  let consume :: Streamly.SerialT IO Text -> IO Text
      consume stream =
        stream
          & Streamly.map (`Text.snoc` '\n')
          & Streamly.foldl' (<>) mempty
          & fmap Text.stripEnd

  Async.concurrently
    do consume stdoutStream
    do consume stderrStream


-- | Ignore output from the previously run command.
receive_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
receive_ ghci = liftIO do
  (stdoutStream, stderrStream) <- receiveStreaming ghci

  Async.concurrently_
    do Streamly.drain stdoutStream
    do Streamly.drain stderrStream


-- | Send Ctrl-C (@SIGINT@) to GHCi session. Useful for interrupting
-- long-running commands.
cancel
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
cancel ghci = liftIO $ Process.interruptProcessGroupOf (processHandle ghci)


-- | Interact with the GHCi session directly via @stdin@ and @stdout@ +
-- @stderr@. Useful for debugging and experimenting.
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


-- | Take lock on GHCi session while performing action, preventing other actions
-- (which respect the lock) from accessing it concurrently.
withLock :: MonadUnliftIO m => Ghci s -> m a -> m a
withLock Ghci{lockTMVar} action = withRunInIO \unliftIO -> do
  let acquire = STM.atomically $ STM.takeTMVar lockTMVar
  let release = STM.atomically $ STM.putTMVar lockTMVar ()
  Exception.bracket_ acquire release (unliftIO action)


-- | Prompt separator text
separator :: Integer -> Text
separator n = Text.pack ("__sidekick__" <> show n <> "__")
