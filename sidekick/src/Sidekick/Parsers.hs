{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Test parsers

module Sidekick.Parsers
  ( Message (..)
  , LoadingMessage (..)
  , DiagnosticMessage (..)
  , Severity (..)
  , Location (..)
  , Position (..)
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

import Relude.Extra.Tuple (dup)
import Relude.Unsafe (read)
import Prelude hiding (cycle, lines)

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
  , spanBegin :: Position
  , spanEnd :: Position
  }


data Position = Position
  { line :: Natural
  , column :: Natural
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
  -- src/Sidekick/Parsers.hs:265:1-14: warning: [-Wunused-top-binds (in -Wextra, -Wunused-binds)]
  --     Defined but not used: ‘parsePositions’
  --     |
  -- 265 | parsePositions = point <|> singleLine <|> multiLine
  --     | ^^^^^^^^^^^^^^
  normal = do
    location <- do
      file <- toString <$> Megaparsec.takeWhileP Nothing (/= ':')
      _ <- Megaparsec.char ':'
      (spanBegin, spanEnd) <- parsePositions
      pure $ Just Location{file, spanBegin, spanEnd}
    _ <- Megaparsec.char ' '
    severity <-
      Megaparsec.optional (Megaparsec.lookAhead (Megaparsec.string "warning: ")) <&> \case
        Just _ -> Warning
        Nothing -> Error
    message <- mconcat <$> takeRestLine `Megaparsec.endBy1` Megaparsec.char '\n'
    pure DiagnosticMessage{severity, location, message}

  -- <no location info>: can't find file: FILENAME
  cantFindFile = do
    _ <- Megaparsec.string "<no location info>: can't find file: "
    file <- takeRestLine
    pure DiagnosticMessage
      { severity = Error
      , location = Nothing
      , message = "can't find file: " <> file
      }

  -- <no location info>: error:
  err = do
    _ <- Megaparsec.string "<no location info>: error:\n"
    message <- parseIndentedLines
    pure DiagnosticMessage
      { severity = Error
      , location = Nothing
      , message
      }

  -- Module imports form a cycle:
  --   module `Module' (Module.hs) imports itself
  cycle = do
    _ <- Megaparsec.string "Module imports form a cycle:\n"
    message <- parseIndentedLines
    pure DiagnosticMessage
      { severity = Error
      , location = Nothing
      , message
      }


-- Loaded GHCi configuration from C:\Neil\ghcid\.ghci
parseLoadConfigMessage :: Parser LoadConfigMessage
parseLoadConfigMessage = do
  _ <- Megaparsec.string "Loaded GHCi configuration from "
  path <- toString <$> takeRestLine
  pure LoadConfigMessage{path}


parseCwd :: Parser FilePath
parseCwd = do
  _ <- Megaparsec.string "current working directory:"
  Megaparsec.space1
  cwd <- takeRestLine
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


-- 1:2:
-- 1:2-4:
-- (1,2)-(3,4):
parsePositions :: Parser (Position, Position)
parsePositions = Megaparsec.try point <|> singleLine <|> multiLine
  where
  point = do
    line <- parseNatural
    _ <- Megaparsec.char ':'
    column <- parseNatural
    _ <- Megaparsec.char ':'
    pure (dup Position{line, column})

  singleLine = do
    line <- parseNatural
    _ <- Megaparsec.char ':'
    columnBegin <- parseNatural
    _ <- Megaparsec.char '-'
    columnEnd <- parseNatural
    _ <- Megaparsec.char ':'
    pure
      ( Position{line, column = columnBegin}
      , Position{line, column = columnEnd}
      )

  multiLine = do
    (lineBegin, columnBegin) <- parseNaturalPair
    _ <- Megaparsec.char '-'
    (lineEnd, columnEnd) <- parseNaturalPair
    _ <- Megaparsec.char ':'
    pure
      ( Position{line = lineBegin, column = columnBegin}
      , Position{line = lineEnd, column = columnEnd}
      )

  parseNatural = read . toString <$> Megaparsec.some Megaparsec.digitChar

  parseNaturalPair =
    Megaparsec.between (Megaparsec.char '(') (Megaparsec.char ')') do
      x <- parseNatural
      _ <- Megaparsec.char ','
      y <- parseNatural
      pure (x, y)


parseIndentedLines :: Parser Text
parseIndentedLines = do
  let isHspace c = Char.isSpace c && c /= '\n' && c /= '\r'
  let parseLine = Megaparsec.takeWhile1P Nothing isHspace <> takeRestLine
  lines <- parseLine `Megaparsec.sepBy` Megaparsec.char '\n'
  pure (Text.unlines lines)


takeRestLine :: Parser Text
takeRestLine = Megaparsec.takeWhile1P Nothing (/= '\n')
