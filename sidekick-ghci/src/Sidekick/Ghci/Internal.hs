{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO
import qualified System.Process as Process
import qualified System.Random as Random


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
withGhci command action = withRunInIO \unliftIO ->
  Process.withCreateProcess processConfig \i o e p -> unliftIO $ setup i o e p
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
  setup i o e processHandle =
    case (i, o, e) of
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
        commandTVar <- liftIO $ STM.newTVarIO ""
        separatorTVar <- liftIO $ STM.newTVarIO ""

        let ghci = Ghci
              { stdinHandle
              , stdoutHandle
              , stderrHandle
              , processHandle
              , commandTVar
              , separatorTVar
              }

        liftIO $ IO.hSetBuffering stdinHandle IO.LineBuffering
        liftIO $ IO.hSetBuffering stdoutHandle IO.LineBuffering
        liftIO $ IO.hSetBuffering stderrHandle IO.LineBuffering

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
        liftIO $ Exception.throwIO (userError "Failed to create GHCi handles")


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
cancel ghci = liftIO $ Process.interruptProcessGroupOf (processHandle ghci)


-- | Run a command in GHCi
send
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> Text
  -- ^ GHCi command or Haskell expression
  -> m ()
send ghci command = liftIO do
  random <- Random.randomRIO @Int (0, 1_000_000)

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
receive ghci = liftIO do
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

  Async.concurrently
    do stream (stdoutHandle ghci)
    do stream (stderrHandle ghci)


-- | Ignore output from the previously run command
receive_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
receive_ ghci = liftIO do
  separator <- STM.atomically $ STM.readTVar (separatorTVar ghci)

  let stream :: Handle -> IO ()
      stream handle =
        Streamly.repeatM (Text.hGetLine handle)
          & Streamly.takeWhile (/= separator)
          & Streamly.drain

  Async.concurrently_
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
