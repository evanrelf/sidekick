-- |
-- Module:     Sidekick.Ghci
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Interact with a live GHCi session

module Sidekick.Ghci
  ( Ghci

    -- * Start GHCi session
  , withGhci

    -- * Operations
  , run
  , run_
  , cancel
  )
where

import Sidekick.Ghci.Internal
