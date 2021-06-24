{-# LANGUAGE DuplicateRecordFields #-}

-- TODO: Test parsers

module Sidekick.Parsers
  ( Message (..)
  , LoadingMessage (..)
  , DiagnosticMessage (..)
  , Severity (..)
  , LoadConfigMessage (..)
  , parseMessage
  , parseLoadingMessage
  , parseDiagnosticMessage
  , parseLoadConfigMessage
  , parseCwd
  , parseModules
  , parseModule
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


-- Based on Neil Mitchell's parsers in `ghcid`:
-- https://github.com/ndmitchell/ghcid/blob/master/src/Language/Haskell/Ghcid/Parser.hs


type Parser = Megaparsec.Parsec Void Text


data Message
  = Loading LoadingMessage
  | Diagnostic DiagnosticMessage
  | LoadConfig LoadConfigMessage


data LoadingMessage = LoadingMessage
  { moduleName :: Text
  , file :: FilePath
  }


data DiagnosticMessage = DiagnosticMessage
  { severity :: Severity
  , file :: FilePath
  , positionBegin :: (Natural, Natural)
  , positionEnd :: (Natural, Natural)
  , message :: Text
  }


data Severity
  = Warning
  | Error


newtype LoadConfigMessage = LoadConfigMessage
  { path :: FilePath
  }


parseMessage :: Parser Message
parseMessage = asum
  [ Loading <$> parseLoadingMessage
  , Diagnostic <$> parseDiagnosticMessage
  , LoadConfig <$> parseLoadConfigMessage
  ]


-- [1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )
parseLoadingMessage :: Parser LoadingMessage
parseLoadingMessage = undefined


parseDiagnosticMessage :: Parser DiagnosticMessage
parseDiagnosticMessage = undefined


-- Loaded GHCi configuration from C:\Neil\ghcid\.ghci
parseLoadConfigMessage :: Parser LoadConfigMessage
parseLoadConfigMessage = undefined


parseCwd :: Parser FilePath
parseCwd = do
  _ <- Megaparsec.string "current working directory:"
  Megaparsec.space1
  cwd <- Megaparsec.takeWhile1P Nothing (/= '\n')
  pure (Text.unpack cwd)


parseModules :: Parser [(Text, FilePath)]
parseModules = Megaparsec.many (parseModule <* Megaparsec.newline)


parseModule :: Parser (Text, FilePath)
parseModule = do
  moduleName <- Megaparsec.takeWhile1P Nothing (not . Char.isSpace)
  Megaparsec.space1
  _ <- Megaparsec.char '('
  Megaparsec.space1
  modulePath <- Megaparsec.takeWhile1P Nothing (/= ',')
  _ <- Megaparsec.takeWhile1P Nothing (/= ')')
  _ <- Megaparsec.char ')'
  pure (moduleName, Text.unpack modulePath)
