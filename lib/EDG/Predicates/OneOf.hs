
module EDG.Predicates.OneOf where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Newtype
import Control.Newtype.Util

-- | The set of elements our target has to be a member of.
newtype OneOf a = OneOf (Set a)
  deriving (Show, Read, Eq)

instance Newtype (OneOf a) (Set a) where
  pack = OneOf
  unpack (OneOf a) = a

instance (Ord a) => AsPredicate (OneOf a) where
  type PredicateDomain (OneOf a) = a
  asPredicate (OneOf s) e = Set.member e s

instance (Ord a) => SATAblePredicate (OneOf a) where
  isSAT = under' $ not . Set.null

instance (Ord a) => CollapseablePredicate (OneOf a) where
  collapse (OneOf s)
    | Set.size s == 1 = Just $ Set.findMin s
    | otherwise       = Nothing

instance (Ord a) => LiftablePredicate (OneOf a) where
  liftPredicate a = OneOf $ Set.singleton a

instance (Ord a) => BottomPredicate (OneOf a) where
  isBottom _ = False

instance (Ord a) => PartialOrd (OneOf a) where
  leq (OneOf a) (OneOf b) = b `Set.isSubsetOf` a

instance (Ord a) => JoinSemiLattice (OneOf a) where
  (\/) (OneOf a) (OneOf b) = OneOf $ Set.intersection a b

instance (Ord a) => MeetSemiLattice (OneOf a) where
  (/\) (OneOf a) (OneOf b) = OneOf $ Set.union a b

instance (Ord a) => BoundedMeetSemiLattice (OneOf a) where
  top = OneOf Set.empty

-- | The join of a OneOf and any other predicate is the OneOf filtered of all
--   elements that don't meet the other predicate.
joinOneOf :: (PredDom a ~ e, AsPredicate a) => a -> OneOf e -> OneOf e
joinOneOf p (OneOf s) = OneOf $ Set.filter (asPredicate p) s

class (AsPredicate t) => OneOfConstraint t where
  oneOf :: [PredDom t] -> t

is :: (AsPredicate t,OneOfConstraint t) =>  PredDom t -> t
is t = oneOf [t]

instance (Ord t) => OneOfConstraint (OneOf t) where
  oneOf l = OneOf $ Set.fromList l
