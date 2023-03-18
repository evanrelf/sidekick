{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Sidekick.Ghci.Json
  ( Message (..)
  , Span (..)
  , Severity (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson


data Message = Message
  { span :: Maybe Span
  , doc :: Text
  , severity :: Maybe Severity
  , reason :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


data Span = Span
  { file :: FilePath
  , startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


data Severity
  = SevOutput
  | SevWarning
  | SevError
  | SevFatal
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
