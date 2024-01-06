{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sidekick.Hie
  ( readHieFile
  , moduleName
  , sourceCode
  , Error (..)
  )
where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Iface.Ext.Binary qualified as Ghc
  ( HieFileResult (..)
  , HieHeader
  , readHieFileWithVersion
  )
import GHC.Iface.Ext.Types qualified as Ghc (HieFile (..), hieVersion)
import GHC.Types.Name.Cache qualified as Ghc (initNameCache)
import GHC.Unit.Module.Name qualified as Ghc (moduleNameString)
import GHC.Unit.Types qualified as Ghc (GenModule (..))

data Error
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

moduleName :: Ghc.HieFile -> Text
moduleName hieFile =
  Text.pack (Ghc.moduleNameString hieFile.hie_module.moduleName)

sourceCode :: Ghc.HieFile -> Text
sourceCode hieFile = Text.decodeUtf8 hieFile.hie_hs_src
