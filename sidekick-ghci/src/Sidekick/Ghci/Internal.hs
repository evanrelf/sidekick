{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:     Sidekick.Ghci.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2023 Evan Relf
-- Maintainer: evan@evanrelf.com

module Sidekick.Ghci.Internal where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Function ((&))
import Data.Text (Text)
import System.IO (Handle)
import Prelude hiding (interact)

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO
import qualified System.Process as Process

#if !MIN_VERSION_streamly(0,8,0)
import qualified Streamly
#define fromZipAsync zipAsyncly
#define fromEffect yieldM
#endif


-- | GHCi session handle.
data Ghci s = Ghci
  { stdinHandle :: Handle
    -- ^ Handle for GHCi session's @stdin@ stream

  , stdoutHandle :: Handle
    -- ^ Handle for GHCi session's @stdout@ stream

  , stderrHandle :: Handle
    -- ^ Handle for GHCi session's @stderr@ stream

  , processHandle :: Process.ProcessHandle
    -- ^ Process handle for GHCi session

  , promptNumberTVar :: STM.TVar Integer
    -- ^ Mutable reference to last prompt number
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
  setup i o e processHandle = do
    (stdinHandle, stdoutHandle, stderrHandle) <-
      case (i, o, e) of
        (Just i', Just o', Just e') -> pure (i', o', e')
        _ -> Exception.throwIO (userError "Failed to create GHCi handles")

    promptNumberTVar <- STM.newTVarIO 0

    let ghci = Ghci
          { stdinHandle
          , stdoutHandle
          , stderrHandle
          , processHandle
          , promptNumberTVar
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


-- | Run a command in GHCi, collecting its output.
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


-- | Run a command in GHCi, ignoring its output.
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
    pure promptNumber

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
  n <- STM.atomically $ STM.readTVar (promptNumberTVar ghci)

  let streamHandle :: Streamly.MonadAsync m => Handle -> Streamly.SerialT m Text
      streamHandle handle =
        hGetLines handle
          & Streamly.takeWhile (/= separator n)

  pure
    ( streamHandle (stdoutHandle ghci)
    , streamHandle (stderrHandle ghci)
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

  let foldStream :: Streamly.SerialT IO Text -> IO Text
      foldStream stream =
        stream
          & Streamly.map (`Text.snoc` '\n')
          & Streamly.foldl' (<>) mempty
          & fmap Text.stripEnd

  let stream :: Streamly.ZipAsyncM IO (Text, Text)
      stream = (,)
        <$> Streamly.fromEffect (foldStream stdoutStream)
        <*> Streamly.fromEffect (foldStream stderrStream)

  Streamly.fromZipAsync stream
    & Streamly.toList
    & fmap head


-- | Ignore output from the previously run command.
receive_
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m ()
receive_ ghci = liftIO do
  (stdoutStream, stderrStream) <- receiveStreaming ghci

  Streamly.drain $ stdoutStream `Streamly.parallel` stderrStream


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
  hGetLines IO.stdin
    & Streamly.mapM (run ghci)
    & Streamly.trace (\(out, err) -> do
        unless (Text.null err) $ Text.hPutStrLn IO.stderr err
        unless (Text.null out) $ Text.hPutStrLn IO.stdout out
      )
    & Streamly.drain


-- | Prompt separator text
separator :: Integer -> Text
separator n = Text.pack ("__sidekick__" <> show n <> "__")


hGetLines
  :: MonadIO (t m)
  => Streamly.IsStream t
  => Streamly.MonadAsync m
  => Handle
  -> t m Text
hGetLines handle = do
  liftIO $ IO.hSetBuffering handle IO.LineBuffering
  flip Streamly.unfoldrM () \() -> liftIO do
    eof <- IO.hIsEOF handle
    if eof then
      pure Nothing
    else do
      line <- Text.hGetLine handle
      pure $ Just (line, ())
