{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import GHC.SyntaxHighlighter qualified as Lexer
import Sidekick.Hie qualified as Hie
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "sidekick-hie"
    [ HUnit.testCase "It works" do
        hieFile <- Hie.readHieFile ".hie/test/Example.hie"

        Hie.sourcePath hieFile @?= "test/Example.hs"

        Hie.moduleName hieFile @?= "Example"

        let expectedSourceCode = unlines
              [ "module Example (answer) where"
              , "answer :: Num a => a"
              , "answer = 42"
              ]

        let expectedTokens =
              [ "KeywordTok", "ConstructorTok", "SymbolTok", "VariableTok"
              , "SymbolTok", "KeywordTok", "VariableTok", "SymbolTok"
              , "ConstructorTok", "VariableTok", "SymbolTok", "VariableTok"
              , "VariableTok", "SymbolTok", "IntegerTok"
              ]

        Hie.sourceCode hieFile @?= expectedSourceCode

        let tokenName = (\(t, _l) -> show @Text t)

        do
          let actualTokens = fmap tokenName (Hie.tokensWithLocations hieFile)
          actualTokens @?= expectedTokens

        do
          let actualTokens =
                fmap
                  (fmap tokenName)
                  (Lexer.tokenizeHaskellLoc expectedSourceCode)
          actualTokens @?= Just expectedTokens
    ]
