{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.String.Interpolate (i)

import qualified Sidecar.Ghci as Ghci
import qualified Test.Hspec as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "ghci" do
    let query command = Ghci.withGhci "ghci" \ghci -> Ghci.query ghci command

    basicExpressions query

  Hspec.describe "cabal repl" do
    let query command = Ghci.withGhci "cabal repl" \ghci -> Ghci.query ghci command

    basicExpressions query


basicExpressions :: (Text -> IO (Text, Text)) -> Hspec.SpecWith ()
basicExpressions query =
  Hspec.it "should evaluate basic expressions" do
    query "(2 :: Int) + 2" `Hspec.shouldReturn` ("4", "")
    query [i|("foo" :: String) == "bar"|] `Hspec.shouldReturn` ("False", "")
