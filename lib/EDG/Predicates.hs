
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


