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

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


-- | Return GHCi session's current working directory, parsed from @:show paths@
-- command
getCwd
  :: Ghci s
  -- ^ GHCi session handle
  -> IO FilePath
  -- ^ GHCi session's current working directory
getCwd ghci = do
  (rawPaths, _) <- run ghci ":show paths"

  undefined


-- | Return currently loaded modules, parsed from @:show modules@ command
getModules
  :: Ghci s
  -- ^ GHCi session handle
  -> IO [(Text, FilePath)]
  -- ^ Module name and file path for currently loaded modules
getModules ghci = do
  (rawModules, _) <- run ghci ":show modules"

  undefined


type Parser = Megaparsec.Parsec Void Text


type ParseErrorBundle = Megaparsec.ParseErrorBundle Void Text


parseCwd :: Text -> Parser FilePath
parseCwd input = undefined


parseModule :: Text -> Parser (Text, FilePath)
parseModule input = undefined
