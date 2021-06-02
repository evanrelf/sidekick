{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:     Sidekick.Ghci
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Interact with a live GHCi session

module Sidekick.Ghci
  ( Ghci

    -- * Start GHCi session
  , withGhci

    -- * Operations
  , run
  , run_
  , cancel
  , getCwd
  , getModules
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
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Void (Void)

import qualified Control.Exception as Exception
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


-- | Return GHCi session's current working directory, parsed from the
-- @:show paths@ command
getCwd
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m FilePath
  -- ^ GHCi session's current working directory
getCwd ghci = do
  (rawPaths, _) <- run ghci ":show paths"

  case Megaparsec.parse parseCwd "<interactive>" rawPaths of
    Left err -> liftIO $ Exception.throwIO err
    Right x -> pure x


-- | Return currently loaded modules, parsed from the @:show modules@ command
getModules
  :: MonadIO m
  => Ghci s
  -- ^ GHCi session handle
  -> m [(Text, FilePath)]
  -- ^ Module name and file path for currently loaded modules
getModules ghci = do
  (rawModules, _) <- run ghci ":show modules"

  forM (Text.lines rawModules) \rawModule ->
    case Megaparsec.parse parseModule "<interactive>" rawModule of
      Left err -> liftIO $ Exception.throwIO err
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
