{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Sub.Unagi
  (
  -- * Sub carrier
    runSub
  , SubC (..)
  -- * Sub effect
  , module Control.Effect.Sub
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Applicative (Alternative)
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Sub
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans)

import qualified Control.Concurrent.Chan.Unagi as Unagi


runSub :: Unagi.OutChan msg -> SubC msg m a -> m a
runSub outChan = runReader outChan . runSubC


newtype SubC msg m a = SubC { runSubC :: ReaderC (Unagi.OutChan msg) m a }
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


instance (MonadIO m, Algebra sig m) => Algebra (Sub msg :+: sig) (SubC msg m) where
  alg hdl sig ctx = SubC $ case sig of
    L Sub -> ReaderC $ \outChan -> do
      msg <- liftIO (Unagi.readChan outChan)
      pure (msg <$ ctx)

    R other ->
      alg (runSubC . hdl) (R other) ctx
