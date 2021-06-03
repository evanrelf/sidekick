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

    -- * High-level operations
    -- | High-level wrappers for 'send', 'receive', and 'receive_'. Calls to
    -- 'send' are always followed by 'receive' or 'receive_' to ensure the GHCi
    -- session is in a good state for the next command.
  , run
  , run_
  , cancel

    -- * Low-level operations
    -- | Low-level primitives for more direct manipulation of the GHCi session,
    -- providing no checks or guarantees that you maintain a good state.
  , send
  , receive
  , receive_

    -- * Debugging
  , interact
  )
where

import Sidekick.Ghci.Internal
import Prelude hiding (interact)
