{-# LANGUAGE OverloadedLists #-}

module Sidekick (main) where

-- main :: IO ()
-- main = pass

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, runM)
import Control.Carrier.Sub.Unagi (Sub, runSub)
import Control.Carrier.Pub.Unagi (Pub, runPub)
import Optics ((%), (^.))
import Sidekick.Options (Options)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Optics.TH
import qualified Sidekick.FSNotify as FSNotify
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI


main :: IO ()
main = do
  options <- Options.getOptions

  (uiInChan, uiOutChan) <- Unagi.newChan
  (ghciInChan, ghciOutChan) <- Unagi.newChan
  (fsnotifyInChan, fsnotifyOutChan) <- Unagi.newChan

  racing
    [ startUI
    , startGhci
    , startFSNotify
    ]


racing :: NonEmpty (IO a) -> IO a
racing = go [] . toList
  where
  go threads [] =
    snd <$> Async.waitAny threads
  go threads (action : actions) =
    Async.withAsync action \thread -> go (thread : threads) actions


racing_ :: NonEmpty (IO ()) -> IO ()
racing_ = void . racing


startUI :: IO ()
startUI =
  UI.start
    & runSub (undefined :: Unagi.OutChan UI.Event)
    & runM


startGhci :: IO ()
startGhci = do
  let command = "cabal repl"

  Ghci.withGhci command \_ghci -> do
    -- TODO
    pass


startFSNotify :: IO ()
startFSNotify = do
  FSNotify.start "TODO"
    & runPub (undefined :: Unagi.InChan FilePath)
    & runM
