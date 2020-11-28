{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Sidekick (main) where

import Optics ((%), (^.))
import Sidekick.Options (Options)

import qualified Brick.BChan as Brick
import qualified Control.Concurrent.Async as Async
import qualified Optics.TH
import qualified Sidekick.FSNotify as FSNotify
import qualified Sidekick.Ghci as Ghci
import qualified Sidekick.Options as Options
import qualified Sidekick.UI as UI


data Env = Env
  { options :: Options
  , uiEventChannel :: Brick.BChan UI.Event
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''Env


main :: IO ()
main = do
  options <- Options.getOptions

  uiEventChannel <- Brick.newBChan 10

  let env = Env{options, uiEventChannel}

  start env


start :: Env -> IO ()
start env =
  Async.withAsync (startUI env) \uiThread ->
  Async.withAsync (startGhci env) \ghciThread ->
  Async.withAsync (startFSNotify env) \fsnotifyThread ->
  void $ Async.waitAny
    [ uiThread
    , ghciThread
    , fsnotifyThread
    ]


startUI :: Env -> IO ()
startUI env = do
  UI.start (env ^. #uiEventChannel)


startGhci :: Env -> IO ()
startGhci env = do
  let command = env ^. #options % #command & fromMaybe "cabal repl"

  Ghci.withGhci command \ghci -> do
    undefined


startFSNotify :: Env -> IO ()
startFSNotify env = do
  FSNotify.start undefined
