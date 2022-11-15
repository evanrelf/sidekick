{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Sidekick.Lsp
  ( start
  )
where

import Data.Default (def)

import qualified Data.Aeson as Aeson
import qualified Language.LSP.Server as Lsp
import qualified Language.LSP.Types as Lsp


type Config = ()


type Env = ()


type M = IO


start :: MonadIO m => m Int
start = liftIO do
  Lsp.runServer Lsp.ServerDefinition
    { Lsp.defaultConfig
    , Lsp.onConfigurationChange
    , Lsp.doInitialize
    , Lsp.staticHandlers
    , Lsp.interpretHandler
    , Lsp.options
    }

  where
  defaultConfig :: Config
  defaultConfig = ()

  onConfigurationChange :: Config -> Aeson.Value -> Either Text ()
  onConfigurationChange config json = Right ()

  doInitialize
    :: Lsp.LanguageContextEnv Config
    -> Lsp.Message Lsp.Initialize
    -> IO (Either Lsp.ResponseError Env)
  doInitialize languageContextEnv initializeRequest = pure $ Right ()

  staticHandlers :: Lsp.Handlers M
  staticHandlers = mempty

  interpretHandler :: Env -> M Lsp.<~> IO
  interpretHandler env =
    Lsp.Iso
      { Lsp.forward = identity
      , Lsp.backward = identity
      }

  options :: Lsp.Options
  options = def
