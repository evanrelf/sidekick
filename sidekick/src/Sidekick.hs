module Sidekick (main) where

import qualified Sidekick.Lsp as Lsp
import qualified Sidekick.Options as Options


main :: IO ()
main = do
  options <- Options.getOptions
  print =<< Lsp.start
