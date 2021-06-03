module Sidekick (main) where

import qualified Sidekick.Options as Options


main :: IO ()
main = do
  _options <- Options.getOptions

  pass
