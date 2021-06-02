-- | Interact with a live GHCi session

module Sidekick.Ghci
  (
  -- * Start GHCi session
    withGhci
  -- * Operations
  , run
  , run_
  , cancel
  , getCwd
  , getModules
  -- * Other
  , Ghci
  )
where

import Sidekick.Ghci.Internal
  ( Ghci
  , withGhci
  , run
  , run_
  , cancel
  )

import Control.Monad (forM)
import Data.Text (Text)
import Data.Void (Void)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified UnliftIO.Exception as Exception


-- | Return GHCi session's current working directory, parsed from the
-- @:show paths@ command
getCwd
  :: Ghci s
  -- ^ GHCi session handle
  -> IO FilePath
  -- ^ GHCi session's current working directory
getCwd ghci = do
  (rawPaths, _) <- run ghci ":show paths"

  case Megaparsec.parse parseCwd "<interactive>" rawPaths of
    Left err -> Exception.throwIO err
    Right x -> pure x


-- | Return currently loaded modules, parsed from the @:show modules@ command
getModules
  :: Ghci s
  -- ^ GHCi session handle
  -> IO [(Text, FilePath)]
  -- ^ Module name and file path for currently loaded modules
getModules ghci = do
  (rawModules, _) <- run ghci ":show modules"

  forM (Text.lines rawModules) \rawModule ->
    case Megaparsec.parse parseModule "<interactive>" rawModule of
      Left err -> Exception.throwIO err
      Right x -> pure x


type Parser = Megaparsec.Parsec Void Text


parseCwd :: Parser FilePath
parseCwd = do
  _ <- Megaparsec.string "current working directory:"
  Megaparsec.space1
  cwd <- Megaparsec.takeWhile1P Nothing (/= '\n')
  pure (Text.unpack cwd)


parseModule :: Parser (Text, FilePath)
parseModule = do
  moduleName <- Megaparsec.takeWhile1P Nothing (not . Char.isSpace)
  Megaparsec.space1
  _ <- Megaparsec.char '('
  Megaparsec.space1
  modulePath <- Megaparsec.takeWhile1P Nothing (/= ',')
  pure (moduleName, Text.unpack modulePath)
