{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MonadSymbolic where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S
import qualified Data.SBV.Dynamic as S

import Control.Monad.Trans.Class

type Symbolic = S.Symbolic
type SBV      = S.SBV
type SymWord  = S.SymWord

class Monad m => MonadSymbolic m where
  sBool     :: String -> m (SBV Bool)
  sInteger  :: String -> m (SBV Integer)
  sFloat    :: String -> m (SBV Float)
  free      :: SymWord a => String -> m (SBV a)

-- Just so i can use constrain in more outward monads as well.
class Monad m => MonadConstrain m a where
  constrain :: a -> m ()

-- This is where we're using UndecidableInstances. This can almost certainly
-- cause a softlock if we have some sort of weird state stuff going on but
-- maybe it's fine?
instance (MonadSymbolic m,MonadTrans t, Monad (t m)) => MonadSymbolic (t m) where
  sBool     = lift . sBool
  sInteger  = lift . sInteger
  sFloat    = lift . sFloat
  free      = lift . free

instance (MonadConstrain m (SBV Bool),MonadTrans t, Monad (t m)) => MonadConstrain (t m) (SBV Bool) where
  constrain = lift . constrain

instance MonadSymbolic Symbolic where
  sBool     = S.sBool
  sInteger  = S.sInteger
  sFloat    = S.sFloat
  free      = S.free

instance MonadConstrain Symbolic (SBV Bool) where
  constrain = S.constrain

-- | Allows you to construct an `SBV typ` value for a typ that has a valid
--   `SBV typ` backing it. This lets us be a bit more precise with typechecking
class (SymWord rep) => SBVWrap typ rep | typ -> rep

getRep :: forall typ rep m. (MonadSymbolic m,SBVWrap typ rep) => String -> m (SBV typ)
getRep s = ((S.SBV :: S.SVal -> SBV typ) . S.unSBV) <$> (free s :: m (SBV rep))

reWrap :: forall typ rep. (SBVWrap typ rep) => SBV rep -> SBV typ
reWrap = (S.SBV :: S.SVal -> SBV typ) . S.unSBV
