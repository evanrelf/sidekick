module Main (main) where

import Test.Tasty.HUnit ((@?=))

import qualified Sidekick.Ghci as Ghci
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Timeout as Timeout


main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "sidekick-ghci"
    [ Tasty.testGroup "ghci"
        (fmap ($ "ghci") tests)

    , Tasty.testGroup "cabal repl"
        (fmap ($ "cabal repl sidekick-ghci") tests)
    ]


tests :: [Text -> Tasty.TestTree]
tests =
  [ test_expressions
  , test_commands
  , test_cancellation
  ]


test_expressions :: Text -> Tasty.TestTree
test_expressions ghciCommand =
  HUnit.testCase "evaluates expressions" $ Ghci.withGhci ghciCommand \ghci -> do
    Ghci.run ghci "(2 :: Int) + 2"
      `shouldReturn` ("4", "")

    Ghci.run ghci "(\"foo\" :: String) == \"bar\""
      `shouldReturn` ("False", "")


test_commands :: Text -> Tasty.TestTree
test_commands ghciCommand =
  HUnit.testCase "runs commands" $ Ghci.withGhci ghciCommand \ghci -> do
    Ghci.run ghci ":type fmap"
      `shouldReturn` ("fmap :: Functor f => (a -> b) -> f a -> f b", "")

    Ghci.run ghci ":!echo hello"
      `shouldReturn` ("hello", "")

    Ghci.run ghci ":!echo hello >&2"
      `shouldReturn` ("", "hello")


test_cancellation :: Text -> Tasty.TestTree
test_cancellation ghciCommand =
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
