{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Control.Effect.Pub
  (
  -- * Pub effect
    Pub (..)
  , pub
  -- * Re-exports
  , Algebra
  , Has
  , run
  )
where

import Control.Algebra (Algebra, Has, run, send)
import Data.Kind (Type)


data Pub msg (m :: Type -> Type) k where
  Pub :: msg -> Pub msg m ()


pub :: Has (Pub msg) sig m => msg -> m ()
pub msg = send (Pub msg)
