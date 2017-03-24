
module EDG.Predicates.NoneOf where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import Data.Set (Set)
import qualified Data.Set as Set


import GHC.Generics
import Control.DeepSeq

import Control.Newtype
import Control.Newtype.Util

-- | The set of elements our target has to be a member of
newtype NoneOf a = NoneOf (Set a)
  deriving (Show, Read, Eq, Generic, NFData)

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

instance (Ord a) => PartialOrd (NoneOf a) where
  leq (NoneOf a) (NoneOf b) = a `Set.isSubsetOf` b

instance (Ord a) => JoinSemiLattice (NoneOf a) where
  (\/) (NoneOf a) (NoneOf b) = NoneOf $ Set.union a b

instance (Ord a) => MeetSemiLattice (NoneOf a) where
  (/\) (NoneOf a) (NoneOf b) = NoneOf $ Set.intersection a b

instance (Ord a) => BoundedJoinSemiLattice (NoneOf a) where
  bottom = NoneOf Set.empty

class (AsPredicate t) => NoneOfConstraint t where
  noneOf :: [PredDom t] -> t

isNot :: (AsPredicate t,NoneOfConstraint t) =>  PredDom t  -> t
isNot t = noneOf [t]

instance (Ord t) => NoneOfConstraint (NoneOf t) where
  noneOf = NoneOf . Set.fromList

-- | The meet of any predicate and a NoneOf is the NoneOf filtered of any
--   elements that match the predicate
meetNoneOf :: (AsPredicate a, PredDom a ~ t) => a -> NoneOf t -> NoneOf t
meetNoneOf p (NoneOf n) = NoneOf $ Set.filter (not . asPredicate p) n
