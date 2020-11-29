{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Pub.Unagi
  (
  -- * Pub carrier
    runPub
  , PubC (..)
  -- * Pub effect
  , module Control.Effect.Pub
  -- * Re-exports
  , Unagi.InChan
  , Unagi.OutChan
  , Unagi.newChan
  , Unagi.dupChan
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Applicative (Alternative)
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Pub
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans)

import qualified Control.Concurrent.Chan.Unagi as Unagi


runPub :: Unagi.InChan msg -> PubC msg m a -> m a
runPub inChan = runReader inChan . runPubC


newtype PubC msg m a = PubC { runPubC :: ReaderC (Unagi.InChan msg) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadTrans
    )


instance (MonadIO m, Algebra sig m) => Algebra (Pub msg :+: sig) (PubC msg m) where
  alg hdl sig ctx = PubC $ case sig of
    L (Pub msg) -> ReaderC $ \inChan ->
      ctx <$ liftIO (Unagi.writeChan inChan msg)

    R other ->
      alg (runPubC . hdl) (R other) ctx
