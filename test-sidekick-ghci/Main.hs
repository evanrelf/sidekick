{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.String.Interpolate (i)

import qualified Sidekick.Ghci as Ghci
import qualified Test.Hspec as Hspec
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Timeout as Timeout


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "ghci" do
    mapM_ ($ "ghci") tests

  Hspec.describe "cabal repl" do
    mapM_ ($ "cabal repl") tests


tests :: [Text -> Hspec.SpecWith ()]
tests =
  [ running
  , cancelling
  ]


running :: Text -> Hspec.SpecWith ()
running ghciCommand = do
  let run command = Ghci.withGhci ghciCommand \ghci -> Ghci.run ghci command

  Hspec.it "evaluates expressions" do
    run "(2 :: Int) + 2" `Hspec.shouldReturn` ("4", "")
    run [i|("foo" :: String) == "bar"|] `Hspec.shouldReturn` ("False", "")

  Hspec.it "runs commands" do
    run ":type fmap" `Hspec.shouldReturn` ("fmap :: Functor f => (a -> b) -> f a -> f b", "")
    run ":!echo hello" `Hspec.shouldReturn` ("hello", "")
    run ":!echo hello >&2" `Hspec.shouldReturn` ("", "hello")


cancelling :: Text -> Hspec.SpecWith ()
cancelling ghciCommand = do
  let scenario :: IO (Maybe ())
      scenario = Ghci.withGhci ghciCommand \ghci -> do
        Ghci.run_ ghci "import qualified Control.Concurrent as Concurrent"
        Ghci.run_ ghci ":set -XNumericUnderscores"

        -- If the operation can't be cancelled, it will block until the
        -- countdown reaches zero, then the test will fail
        Timeout.timeout 5_000_000 do
          Async.concurrently_
            -- Pretend GHCi is stuck evaluating an infinite list or something
            do Ghci.run_ ghci "Concurrent.threadDelay 10_000_000"
            -- Give GHCi a head start, then try to cancel the operation
            do Concurrent.threadDelay 100_000 >> Ghci.cancel ghci

  Hspec.it "cancels operations" do
    scenario `Hspec.shouldReturn` Just ()
