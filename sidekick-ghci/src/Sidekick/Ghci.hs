-- |
-- Module:     Sidekick.Ghci
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2024 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Interact with a live GHCi session

module Sidekick.Ghci
  ( Ghci

    -- * Start GHCi session
  , withGhci

    -- * High-level operations
    -- | Safe wrappers around 'send' and 'receive'. Calls to 'send' are always
    -- followed by 'receive' to ensure the GHCi session is in a good state for
    -- the next command.
  , run
  , run_

    -- * Low-level operations
    -- | Primitives for more direct manipulation of the GHCi session, providing
    -- no checks or guarantees that you maintain a good state.
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
