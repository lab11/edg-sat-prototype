
-- Bah, I hate using this, but best I can tell there's no more elegant way to
-- allow people to grow the set of valid types that can be used as ranges
-- without the one use of superclassing. This is kinda galling however.
{-# LANGUAGE UndecidableInstances #-}

module EDG.Predicates.Range where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import EDG.Predicates.Bounds
import EDG.Predicates.Tuple
import EDG.Predicates.Maybe

import Control.Newtype
import Control.Newtype.Util

-- | A range is composed of an upper and lower bound, and can be worked with
data Range a = Range (Maybe (LowerBound a)) (Maybe (UpperBound a))
  deriving (Show, Read)

instance Newtype (Range a) (Maybe (LowerBound a),Maybe (UpperBound a)) where
  pack (l,u) = Range l u
  unpack (Range l u) = (l,u)

instance (SATPred (Range a),Eq a,Eq (LowerBound a),Eq (UpperBound a)) => Eq (Range a) where
  (==) r@(Range l u) r'@(Range l' u')
    | unSAT r && unSAT r' = True
    | otherwise = (l' == l) && (u' == u)

defaultIsSAT :: (Ord a,SATPred (LowerBound a),SATPred (UpperBound a)) => Range a -> Bool
defaultIsSAT (Range Nothing ub) = isSAT ub
defaultIsSAT (Range lb  Nothing) = isSAT lb
defaultIsSAT (Range (Just (LowerBound Inclusive l))(Just (UpperBound Inclusive u)))
  = l <= u
defaultIsSAT (Range (Just (LowerBound _ l)) (Just (UpperBound _ u)))
  = l < u

enumIsSAT :: (Ord a,Enum a,SATPred (LowerBound a),SATPred (UpperBound a)) => Range a -> Bool
enumIsSAT (Range Nothing ub) = isSAT ub
enumIsSAT (Range lb Nothing) = isSAT lb
enumIsSAT (Range (Just (LowerBound Inclusive l)) (Just (UpperBound Inclusive u)))
  = l <= u
enumIsSAT (Range (Just lb) (Just ub))
  = enumIsSAT $ Range (Just $ normalizeEnumLB lb) (Just $ normalizeEnumUB ub)

instance (Ord a) => AsPredicate (Range a) where
  type PredicateDomain (Range a) = a
  asPredicate (Range a b) = (&&) <$> asPredicate a <*> asPredicate b

instance SATAblePredicate (Range Integer) where
  isSAT = enumIsSAT

instance SATAblePredicate (Range Float) where
  isSAT = defaultIsSAT

instance CollapseablePredicate (Range Integer) where
  collapse (Range (Just lb) (Just ub))
    | l == u    = Just l
    | otherwise = Nothing
    where
      (LowerBound _ l) = normalizeEnumLB lb
      (UpperBound _ u) = normalizeEnumUB ub
  collapse _ = Nothing

instance CollapseablePredicate (Range Float) where
  collapse (Range (Just (LowerBound lInc l)) (Just (UpperBound uInc u)))
    | (l == u) && lInc && uInc = Just l
    | otherwise                = Nothing
  collapse _ = Nothing

instance (Ord a) => LiftablePredicate (Range a) where
  liftPredicate a = Range (Just $ LowerBound Inclusive a) (Just $ UpperBound Inclusive a)

-- `SATPred (Range a)` is the term that requires UndecidableInstances.
-- There's no good way to do this since we need to define instances of SATPred
-- differently for Enum instances and non Enum instances.
--
-- The chain of implication should always terminate at the relevant `SATPred a`
-- constraint.
instance (SATPred (Range a),Ord a,Eq (LowerBound a),Eq (UpperBound a)) => PartialOrd (Range a) where
  leq   (Range (Just lb) Nothing) (Range (Just lb') Nothing )
    = lb `leq` lb'
  leq   (Range Nothing (Just ub)) (Range Nothing (Just ub'))
    = ub `leq` ub'
  leq r@(Range (Just lb) (Just ub)) r'@(Range (Just lb') (Just ub'))
    | unSAT r' = True   -- If r' is Top then r is either Top or less than top
    | unSAT r  = False  -- if unSAT r && isSAT r' then r == Top and r' /= Top
    | otherwise = (lb `leq` lb') && (ub `leq` ub')

instance (Ord a) => JoinSemiLattice (Range a) where
  (\/) (Range l u) (Range l' u') = Range (l \/ l') (u \/ u')

instance (Ord a) => MeetSemiLattice (Range a) where
  (/\) (Range l u) (Range l' u') = Range (l /\ l') (u /\ u')

instance (Ord a) => BoundedJoinSemiLattice (Range a) where
  bottom = Range bottom bottom

instance (Ord a,Num a) => BoundedMeetSemiLattice (Range a) where
  top = Range (Just $ LowerBound Inclusive 1) (Just $ UpperBound Inclusive 0)


instance (Ord t) => GTConstraint (Range t) where
  greaterThan   v = Range (Just $ greaterThan v) Nothing
  greaterThanEq v = Range (Just $ greaterThanEq v) Nothing

instance (Ord t) => LTConstraint (Range t) where
  lessThan   v = Range Nothing (Just $ lessThan v)
  lessThanEq v = Range Nothing (Just $ lessThanEq v)

