
module EDG.Predicates.OneOf where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Newtype
import Control.Newtype.Util

-- | The set of elements our target has to be a member of
newtype OneOf a = OneOf (Set a)
  deriving (Show, Read, Eq)

instance Newtype (OneOf a) (Set a) where
  pack = OneOf
  unpack (OneOf a) = a

instance (Ord a) => AsPredicate (OneOf a) where
  type PredicateDomain (OneOf a) = a
  asPredicate (OneOf s) e = Set.member e s

instance (Ord a) => SATAblePredicate (OneOf a) where
  isSAT = under' (not . Set.null)

instance (Ord a) => CollapseablePredicate (OneOf a) where
  collapse (OneOf s)
    | Set.size s == 1 = Just $ Set.findMin s
    | otherwise       = Nothing

instance (Ord a) => PartialOrd (OneOf a) where
  leq (OneOf a) (OneOf b) = b `Set.isSubsetOf` a

instance (Ord a) => JoinSemiLattice (OneOf a) where
  (\/) (OneOf a) (OneOf b) = OneOf $ Set.intersection a b

instance (Ord a) => MeetSemiLattice (OneOf a) where
  (/\) (OneOf a) (OneOf b) = OneOf $ Set.union a b

instance (Ord a) => BoundedMeetSemiLattice (OneOf a) where
  top = OneOf Set.empty

filterOneOf :: (PredDom a ~ e, AsPredicate a) => a -> OneOf e -> OneOf e
filterOneOf p (OneOf s) = OneOf $ Set.filter (asPredicate p) s
