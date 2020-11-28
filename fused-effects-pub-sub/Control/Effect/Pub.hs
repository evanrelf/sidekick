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


data Pub a (m :: Type -> Type) k where
  Pub :: a -> Pub a m ()


pub :: Has (Pub a) sig m => a -> m ()
pub x = send (Pub x)
