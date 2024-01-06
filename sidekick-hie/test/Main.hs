{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Sidekick.Hie qualified as Hie
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit

main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "sidekick-hie"
    [ HUnit.testCase "It works" do
        hieFile <- Hie.readHieFile ".hie/Sidekick/Hie.hie"
        Hie.sourcePath hieFile @=? "src/Sidekick/Hie.hs"
        Hie.moduleName hieFile @=? "Sidekick.Hie"
    ]
