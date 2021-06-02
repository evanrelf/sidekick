module Main (main) where

import Test.Tasty.HUnit ((@?=))

import qualified Sidekick.Ghci as Ghci
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Directory as Directory
import qualified UnliftIO.Environment as Environment
import qualified UnliftIO.Timeout as Timeout


main :: IO ()
main = do
  -- Don't run tests in parallel by default, because parallel `cabal repl`
  -- sessions conflict
  whenNothingM_ (Environment.lookupEnv "TASTY_NUM_THREADS") do
    Environment.setEnv "TASTY_NUM_THREADS" "1"

  let modules =
        [ ("Sidekick.Ghci", "src/Sidekick/Ghci.hs")
        , ("Sidekick.Ghci.Internal", "src/Sidekick/Ghci/Internal.hs")
        ]

  Tasty.defaultMain $ Tasty.testGroup "sidekick-ghci"
    [ Tasty.testGroup "ghci" $ fmap ($ "ghci") $
        [ test_expressions
        , test_commands
        , test_cancel
        , test_getCwd
        , test_getModules []
        ]

    , Tasty.testGroup "cabal repl" $ fmap ($ "cabal repl sidekick-ghci") $
        [ test_expressions
        , test_commands
        , test_cancel
        , test_getCwd
        , test_getModules modules
        ]
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


test_cancel :: Text -> Tasty.TestTree
test_cancel ghciCommand =
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


test_getCwd :: Text -> Tasty.TestTree
test_getCwd ghciCommand = do
  HUnit.testCase "gets CWD" $ Ghci.withGhci ghciCommand \ghci -> do
    cwd <- Directory.getCurrentDirectory
    Ghci.getCwd ghci `shouldReturn` cwd


test_getModules :: [(Text, FilePath)] -> Text -> Tasty.TestTree
test_getModules modules ghciCommand = do
  HUnit.testCase "gets modules" $ Ghci.withGhci ghciCommand \ghci -> do
    Ghci.getModules ghci `shouldReturn` modules


shouldReturn :: Eq a => Show a => IO a -> a -> HUnit.Assertion
shouldReturn action actual = do
  expected <- action
  expected @?= actual
