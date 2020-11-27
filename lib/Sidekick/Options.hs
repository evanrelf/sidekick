{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sidekick.Options
  ( Options (..)
  , getOptions
  )
where

import qualified Options.Applicative as Options


data Options = Options
  { command :: Maybe Text
  }


getOptions :: IO Options
getOptions = do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseOptions :: Options.Parser Options
parseOptions = do
  command <-
    Options.optional $ Options.strOption $ mconcat
      [ Options.long "command"
      , Options.short 'c'
      , Options.metavar "COMMAND"
      , Options.help "Command to start GHCi session (e.g. `ghci`, `cabal repl`)"
      , Options.hidden
      ]

  pure Options{command}
