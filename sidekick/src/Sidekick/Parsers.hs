{-# LANGUAGE DuplicateRecordFields #-}

-- TODO: Test parsers

module Sidekick.Parsers
  ( Message (..)
  , LoadingMessage (..)
  , DiagnosticMessage (..)
  , Severity (..)
  , Location (..)
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

import Prelude hiding (cycle)

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
  , location :: Maybe Location
  , message :: Text
  }


data Severity
  = Warning
  | Error


data Location = Location
  { file :: FilePath
  , positionBegin :: (Natural, Natural)
  , positionEnd :: (Natural, Natural)
  }


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
parseLoadingMessage = do
  _ <- Megaparsec.char '['
  _ <- Megaparsec.takeWhileP Nothing (/= ']')
  _ <- Megaparsec.char ']'
  _ <- Megaparsec.string " Compiling "
  moduleName <- Megaparsec.takeWhileP Nothing (/= ' ')
  Megaparsec.hspace1
  _ <- Megaparsec.string "( "
  file <- toString <$> Megaparsec.takeWhileP Nothing (/= ',')
  _ <- Megaparsec.takeWhileP Nothing (/= ')')
  _ <- Megaparsec.char ')'
  pure LoadingMessage{moduleName, file}


parseDiagnosticMessage :: Parser DiagnosticMessage
parseDiagnosticMessage = asum
  [ normal
  , cantFindFile
  , err
  , cycle
  ]
  where
  -- GHCi.hs:81:1: Warning: Defined but not used: `foo'
  normal = undefined

  -- <no location info>: can't find file: FILENAME
  cantFindFile = undefined

  -- <no location info>: error:
  err = undefined

  -- Module imports form a cycle:
  --   module `Module' (Module.hs) imports itself
  cycle = undefined


-- Loaded GHCi configuration from C:\Neil\ghcid\.ghci
parseLoadConfigMessage :: Parser LoadConfigMessage
parseLoadConfigMessage = do
  _ <- Megaparsec.string "Loaded GHCi configuration from "
  path <- toString <$> Megaparsec.takeWhile1P Nothing (/= '\n')
  pure LoadConfigMessage{path}


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
