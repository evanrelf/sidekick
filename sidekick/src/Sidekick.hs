module Sidekick (main) where

import Sidekick.Options (Options (..))

import qualified Sidekick.Lsp as Lsp
import qualified Sidekick.Options as Options


main :: IO ()
main = do
  Options{command, directory} <- Options.getOptions
  print =<< Lsp.start
