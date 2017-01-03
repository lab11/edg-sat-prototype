
module EDG.Predicates.NoneOf where

import Algebra.Lattice
import Algebra.AsPredicate

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Newtype
import Control.Newtype.Util

-- | The set of elements our target has to be a member of
newtype NoneOf a = NoneOf (Set a)

instance Newtype (NoneOf a) (Set a) where
  pack = NoneOf
  unpack (NoneOf a) = a

instance (Ord a) => AsPredicate (NoneOf a) where
  type PredicateDomain (NoneOf a) = a
  asPredicate (NoneOf s) e = Set.notMember e s

-- We can't tell whether this is SAT able or unify for most instances
-- it only works for small finite sets. We're better off defining utility
-- functions and leaving it up to the canonical elements that use the noneOf
-- sets.
--
-- Those are probably defined in EDG.Predicates since that's where we keep all
-- the stuff that requires more than one predicate.

instance (Ord a) => JoinSemiLattice (NoneOf a) where
  (\/) (NoneOf a) (NoneOf b) = NoneOf $ Set.union a b

instance (Ord a) => MeetSemiLattice (NoneOf a) where
  (/\) (NoneOf a) (NoneOf b) = NoneOf $ Set.intersection a b

instance (Ord a) => BoundedJoinSemiLattice (NoneOf a) where
  bottom = NoneOf Set.empty


