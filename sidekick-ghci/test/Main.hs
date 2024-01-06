{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty.HUnit ((@?=))

import qualified Sidekick.Ghci as Ghci
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Environment as Environment
import qualified UnliftIO.Timeout as Timeout

main :: IO ()
main = do
  -- Don't run tests in parallel by default, because parallel `cabal repl`
  -- sessions conflict
  whenNothingM_ (Environment.lookupEnv "TASTY_NUM_THREADS") do
    Environment.setEnv "TASTY_NUM_THREADS" "1"

  Tasty.defaultMain $ Tasty.testGroup "sidekick-ghci"
    [ Tasty.testGroup "ghci" $ fmap ($ "ghci") $
        [ testExpressions
        , testCommands
        , testCancel
        ]

    , Tasty.testGroup "cabal repl" $ fmap ($ "cabal repl sidekick-ghci") $
        [ testExpressions
        , testCommands
        , testCancel
        ]
    ]

testExpressions :: Text -> Tasty.TestTree
testExpressions ghciCommand =
  HUnit.testCase "evaluates expressions" $ Ghci.withGhci ghciCommand \ghci -> do
    Ghci.run ghci "(2 :: Int) + 2"
      `shouldReturn` ("4", "")

    Ghci.run ghci "(\"foo\" :: String) == \"bar\""
      `shouldReturn` ("False", "")

    Ghci.run_ ghci ":set -fdiagnostics-color=never"

    Ghci.run ghci "1 + foo"
      `shouldReturn` ("", "\n<interactive>:21:5: error: Variable not in scope: foo")

testCommands :: Text -> Tasty.TestTree
testCommands ghciCommand =
  HUnit.testCase "runs commands" $ Ghci.withGhci ghciCommand \ghci -> do
    Ghci.run_ ghci "import System.IO (hPutStrLn, stderr, stdout)"

    Ghci.run ghci ":type fmap"
      `shouldReturn` ("fmap :: Functor f => (a -> b) -> f a -> f b", "")

    Ghci.run ghci "hPutStrLn stdout \"hello\\nworld\""
      `shouldReturn` ("hello\nworld", "")

    Ghci.run ghci "hPutStrLn stderr \"hello\\nworld\""
      `shouldReturn` ("", "hello\nworld")

    Ghci.run ghci ":set -foo-bar"
      `shouldReturn` ("", "Some flags have not been recognized: -foo-bar")

testCancel :: Text -> Tasty.TestTree
testCancel ghciCommand =
  let
    scenario :: IO (Maybe ())
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

  in
  HUnit.testCase "cancels operations" do
    scenario `shouldReturn` Just ()

shouldReturn :: Eq a => Show a => IO a -> a -> HUnit.Assertion
shouldReturn action actual = do
  expected <- action
  expected @?= actual
