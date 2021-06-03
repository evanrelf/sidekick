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
    -- | High-level wrappers for 'send' and 'receive'. Functions take a lock on
    -- the GHCi session to prevent concurrent access, and calls to 'send' are
    -- always followed by 'receive' to ensure the GHCi session is in a good
    -- state for the next command.
  , run
  , runStreaming
  , run_

    -- * Low-level operations
    -- | Low-level primitives for more direct manipulation of the GHCi session,
    -- providing no checks or guarantees that you maintain a good state.
  , send
  , receive
  , receiveStreaming
  , receive_
  , cancel

    -- * Debugging
  , interact
  )
where

import Sidekick.Ghci.Internal
import Prelude hiding (interact)
