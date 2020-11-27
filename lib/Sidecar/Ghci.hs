-- | Interact with a live GHCi session

module Sidecar.Ghci
  (
  -- * Start GHCi session
    withGhci
  -- * Operations
  , run
  , run_
  )
where

import Sidecar.Ghci.Internal
