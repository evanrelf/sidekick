{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Sidekick (main) where

import Optics ((%), (^.))
import Sidekick.Options (Options)

import qualified Brick.BChan as Brick
import qualified Control.Concurrent.Async as Async
import qualified Optics.TH
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

  Async.race_
    do uiThread env
    do ghciThread env


uiThread :: Env -> IO ()
uiThread env = do
  UI.start (env ^. #uiEventChannel)


ghciThread :: Env -> IO ()
ghciThread env = do
  let command = env ^. #options % #command & fromMaybe "cabal repl"

  Ghci.withGhci command \ghci -> do
    undefined
