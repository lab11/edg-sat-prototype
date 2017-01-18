
module EDG.Predicates
  ( module EDG.Predicates.Bounds
  , module EDG.Predicates.Maybe
  , module EDG.Predicates.OneOf
  , module EDG.Predicates.NoneOf
  , module EDG.Predicates.Predicate
  , module EDG.Predicates.Range
  , module EDG.Predicates.Tuple
  , leqOneNone
  , leqNoneOne
  , flipOneOf
  , flipBoundNoneOf
) where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import EDG.Predicates.Bounds
import EDG.Predicates.Maybe
import EDG.Predicates.OneOf
import EDG.Predicates.NoneOf
import EDG.Predicates.Predicate
import EDG.Predicates.Range
import EDG.Predicates.Tuple

import Data.Set (Set)
import qualified Data.Set as Set

-- | Partial order relation between a NoneOf and OneOf over the same type that
--   fulfills the laws for partial ordering of predicates.
--
--   Basically, if the NoneOf doesn't forbid any of the values in the OneOf
--   then the NoneOf allows for a larger range of valid values.
leqNoneOne :: (Ord t) => NoneOf t -> OneOf t -> Bool
leqNoneOne (NoneOf n) (OneOf o) = Set.null $ Set.intersection n o

-- | Same role as `leqNoneOne` but with the arguments flipped.
--
--   Should follow all the usual laws.
--
--   This one is a bit iffier, since there's a number of cases (other ranges,etc..)
--   limiting the set of elements, and those could actually cause a case where
--   a OneOf is actually less or equal to than a NoneOf. but this is correct
--   for infinite sets.
leqOneNone :: (Ord t) => OneOf t -> NoneOf t -> Bool
leqOneNone (OneOf o) (NoneOf n) = False

-- | Type synonym for a really common set of related constraints.
type BoundNoneOf t = (NoneOf t, LowerBound t, UpperBound t)

type MaybeBoundNoneOf t = (Maybe (NoneOf t), Maybe (LowerBound t),  Maybe (UpperBound t))

-- | flip a OneOf into a NoneOf and some bounds, the input and output should
--   be the same predicate.
flipOneOf :: (Ord t,Enum t) => OneOf t -> BoundNoneOf t
flipOneOf (OneOf s) = (noneOf inverseList, greaterThanEq sMin, lessThanEq sMax)
  where
    sMin = Set.findMin s
    sMax = Set.findMax s
    inverseList = [i | i <- [sMin .. sMax], Set.notMember i s]

-- | flip a NoneOf + some Bounds into the corresponding OneOf, such that the
--   input and output are the same predicate
flipBoundNoneOf :: (Ord t, Enum t) => BoundNoneOf t -> OneOf t
flipBoundNoneOf = undefined -- TODO :: write this bit

-- | Take the meet of some bounded NoneOfs
--
--   a =             >----o----o-------o------oo-----<
--   b = >----o--------o--o--o-----------<
--   c = >----------------o--------------------------<
--   d = >----o-----------o-------------------oo-----<
--
-- c is the piecewise meet (what's happening here) and d is the actual meet.
-- The outer bounds do just do the piecewise meet, but you need to split each
-- noneOf into the set within the other range and without, then recombine them.
instance Ord t => MeetSemiLattice (MaybeBoundNoneOf t) where
  (/\) (Just (NoneOf nas),la,ua) (Just (NoneOf nbs),lb,ub)
    = (Just $ NoneOf ns,la /\ lb,ua /\ ub)
    where
      -- Predicate for the bounds of a
      aPred :: t -> Bool
      aPred = (&&) <$> asPredicate la <*> asPredicate ua
      -- (things outside a but inside b, things in both)
      (oas,ias) = Set.partition bPred (Set.filter aPred nas)
      -- Predicate for the bounds of b
      bPred :: t -> Bool
      bPred = (&&) <$> asPredicate lb <*> asPredicate ub
      -- (things outside b but inside a, things in both)
      (obs,ibs) = Set.partition aPred (Set.filter bPred nbs)
      -- Final composition
      ns = (oas `Set.union` obs) `Set.union` (ias `Set.intersection` ibs)
  (/\) (Nothing,la,ua) (ns,lb,ub) = (ns,la /\ lb,ua /\ ub)
  (/\) (ns,la,ua) (Nothing,lb,ub) = (ns,la /\ lb,ua /\ ub)

