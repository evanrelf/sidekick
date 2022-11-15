{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Sidekick.Lsp
  ( start
  )
where

import Colog.Core ((<&))
import Data.Default (def)

import qualified Colog.Core as Colog
import qualified Data.Aeson as Aeson
import qualified Language.LSP.Logging as Lsp (logToShowMessage)
import qualified Language.LSP.Server as Lsp
import qualified Language.LSP.Types as Lsp


-- https://microsoft.github.io/language-server-protocol/specifications/specification-3-15


type Config = ()



type M = Lsp.LspT Config IO


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
    -> IO (Either Lsp.ResponseError (Lsp.LanguageContextEnv Config))
  doInitialize languageContextEnv initializeRequest =
    pure $ Right languageContextEnv

  staticHandlers :: Lsp.Handlers M
  staticHandlers = mconcat
    [ Lsp.requestHandler Lsp.SInitialize \_request _responder -> pure ()
    , Lsp.notificationHandler Lsp.SInitialized \_notification -> do
        Lsp.logToShowMessage <& "Hello, world!" `Colog.WithSeverity` Colog.Info
    , Lsp.requestHandler Lsp.SShutdown \_request _responder -> pure ()
    , Lsp.notificationHandler Lsp.SExit \_notification -> pure ()
    , Lsp.notificationHandler Lsp.SWorkspaceDidChangeWorkspaceFolders \_notification -> pure ()
    , Lsp.notificationHandler Lsp.SWorkspaceDidChangeConfiguration \_notification -> pure ()
    , Lsp.notificationHandler Lsp.SWorkspaceDidChangeWatchedFiles \_notification -> pure ()
    , Lsp.requestHandler Lsp.SWorkspaceSymbol \_request _responder -> pure ()
    -- , Lsp.requestHandler Lsp.SWorkspaceExecuteCommand \_request _responder -> pure ()
    , Lsp.notificationHandler Lsp.STextDocumentDidOpen \_notification -> pure ()
    , Lsp.notificationHandler Lsp.STextDocumentDidChange \_notification -> pure ()
    , Lsp.notificationHandler Lsp.STextDocumentWillSave \_notification -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentWillSaveWaitUntil \_request _responder -> pure ()
    , Lsp.notificationHandler Lsp.STextDocumentDidSave \_notification -> pure ()
    , Lsp.notificationHandler Lsp.STextDocumentDidClose \_notification -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentCompletion \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SCompletionItemResolve \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentHover \_request _responder -> do
        Lsp.logToShowMessage <& "Hovering!" `Colog.WithSeverity` Colog.Info
    , Lsp.requestHandler Lsp.STextDocumentSignatureHelp \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDeclaration \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDefinition \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentTypeDefinition \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentImplementation \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentReferences \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDocumentHighlight \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDocumentSymbol \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentCodeAction \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentCodeLens \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SCodeLensResolve \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDocumentLink \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SDocumentLinkResolve \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentDocumentColor \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentColorPresentation \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentFormatting \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentRangeFormatting \_request _responder -> pure ()
    -- , Lsp.requestHandler Lsp.STextDocumentOnTypeFormatting \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentRename \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentPrepareRename \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentFoldingRange \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentSelectionRange \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentPrepareCallHierarchy \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SCallHierarchyIncomingCalls \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SCallHierarchyOutgoingCalls \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentSemanticTokens \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentSemanticTokensFull \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentSemanticTokensFullDelta \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.STextDocumentSemanticTokensRange \_request _responder -> pure ()
    , Lsp.requestHandler Lsp.SWorkspaceSemanticTokensRefresh \_request _responder -> pure ()
    ]

  interpretHandler :: Lsp.LanguageContextEnv Config -> M Lsp.<~> IO
  interpretHandler languageContextEnv =
    Lsp.Iso
      { Lsp.forward = Lsp.runLspT languageContextEnv
      , Lsp.backward = liftIO
      }

  options :: Lsp.Options
  options = def
