{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.String.Interpolate (i)

import qualified Sidecar.Ghci as Ghci
import qualified Test.Hspec as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "ghci" do
    let run command = Ghci.withGhci "ghci" \ghci -> Ghci.run ghci command

    basicExpressions run

  Hspec.describe "cabal repl" do
    let run command = Ghci.withGhci "cabal repl" \ghci -> Ghci.run ghci command

    basicExpressions run


basicExpressions :: (Text -> IO (Text, Text)) -> Hspec.SpecWith ()
basicExpressions run =
  Hspec.it "should evaluate basic expressions" do
    run "(2 :: Int) + 2" `Hspec.shouldReturn` ("4", "")
    run [i|("foo" :: String) == "bar"|] `Hspec.shouldReturn` ("False", "")
