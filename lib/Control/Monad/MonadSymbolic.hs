{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MonadSymbolic where

import qualified Data.SBV as S

import Control.Monad.Trans.Class

type Symbolic = S.Symbolic
type SBV = S.SBV

class Monad m => MonadSymbolic m where
  sBool     :: String -> m (SBV Bool)
  sInteger  :: String -> m (SBV Integer)
  constrain :: SBV Bool -> m ()

-- This is where we're using UndecidableInstances. This can almost certainly
-- cause a softlock if we have some sort of weird state stuff going on but
-- maybe it's fine?
instance (MonadSymbolic m,MonadTrans t, Monad (t m)) => MonadSymbolic (t m) where
  sBool     = lift . sBool
  sInteger  = lift . sInteger
  constrain = lift . constrain

instance MonadSymbolic Symbolic where
  sBool     = S.sBool
  sInteger  = S.sInteger
  constrain = S.constrain

-- class EqSymbolic a where
--   sEquals :: MonadSymbolic m => a -> a -> m (SBV Bool)
--
-- -- Also UndecidableInstances is used here, but it should terminate quickly and
-- -- without issue.
-- instance (S.EqSymbolic a) => EqSymbolic a where
--   sEquals a b = return $ (S..==) a b
