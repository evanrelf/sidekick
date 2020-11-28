{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Control.Effect.Sub
  (
  -- * Sub effect
    Sub (..)
  , sub
  -- * Re-exports
  , Algebra
  , Has
  , run
  )
where

import Control.Algebra (Algebra, Has, run, send)
import Data.Kind (Type)


data Sub a (m :: Type -> Type) k where
  Sub :: Sub a m a


sub :: Has (Sub a) sig m => m a
sub = send Sub
