{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Sidekick.Options
  ( Options (..)
  , getOptions
  )
where

import qualified Optics.TH
import qualified Options.Applicative as Options


data Options = Options
  { command :: Maybe Text
  , directory :: Maybe FilePath
  }


Optics.TH.makeFieldLabelsWith Optics.TH.noPrefixFieldLabels ''Options


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

  directory <-
    Options.optional $ Options.strOption $ mconcat
      [ Options.long "directory"
      , Options.short 'd'
      , Options.metavar "DIRECTORY"
      , Options.help "Directory where to start GHCi"
      , Options.hidden
      ]

  pure Options{command, directory}
