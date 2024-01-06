{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sidekick.Hie
  ( readHieFile
  )
where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Iface.Ext.Binary qualified as Ghc
  ( HieFileResult (..)
  , HieHeader
  , readHieFileWithVersion
  )
import GHC.Iface.Ext.Types qualified as Ghc (HieFile (..), hieVersion)
import GHC.Types.Name.Cache qualified as Ghc (initNameCache)

data SidekickHieError
  = IncompatibleVersion Ghc.HieHeader
  deriving stock (Show)
  deriving anyclass (Exception)

readHieFile :: MonadIO m => FilePath -> m Ghc.HieFile
readHieFile path = liftIO do
  let isCorrectVersion (version, _ghcVersion) = version == Ghc.hieVersion
  nameCache <- Ghc.initNameCache 'x' []
  Ghc.readHieFileWithVersion isCorrectVersion nameCache path >>= \case
    Left hieHeader -> Exception.throwIO $ IncompatibleVersion hieHeader
    Right (Ghc.HieFileResult _version _ghcVersion hieFile) -> pure hieFile
