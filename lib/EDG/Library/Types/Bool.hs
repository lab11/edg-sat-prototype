-- Bools can either be true, false, or either

module EDG.Library.Types.Bool where

-- TODO :: At some point make a nicer implementation of this that just keeps
--         track of the four points in the lattice :V

import Algebra.PartialOrd
import Algebra.Lattice
import Algebra.AsPredicate
import Algebra.Constrainable

import Control.Newtype
import Control.Newtype.Util

import EDG.Predicates

import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Exts

-- | This is the canonical type of constraints over boolean variables.
--
--   TODO :: Move to a more efficient representation of this, that doesn't
--           constantly pack and unpack sets.
--
newtype BoolCons = BoolCons {bcOneOf :: OneOf Bool}
  deriving (Show, Read, Eq)

-- TODO :: Better Show, Read instances that mimic the style of the isList
--         representation.

instance Newtype BoolCons (OneOf Bool) where
  pack = BoolCons
  unpack (BoolCons o) = o

instance AsPredicate BoolCons where
  type PredicateDomain BoolCons = Bool
  asPredicate = under' asPredicate

instance SATAblePredicate BoolCons where
  isSAT = under' isSAT

instance CollapseablePredicate BoolCons where
  collapse = under' collapse

instance LiftablePredicate BoolCons where
  liftPredicate = pack . liftPredicate

instance BottomPredicate BoolCons where
  isBottom = (==) bottom

instance PartialOrd BoolCons where
  leq = under2 leq

instance JoinSemiLattice BoolCons where
  (\/) a b = pack $ under2 (\/) a b

instance MeetSemiLattice BoolCons where
  (/\) a b = pack $ under2 (/\) a b

instance BoundedJoinSemiLattice BoolCons where
  bottom = oneOf [True,False]

instance BoundedMeetSemiLattice BoolCons where
  top = pack top

instance OneOfConstraint BoolCons where
  oneOf l = pack $ oneOf l

instance NoneOfConstraint BoolCons where
  noneOf l = pack . pack $ Set.difference (Set.fromList [True,False]) (Set.fromList l)

instance Constrainable Bool where
  type Constraints Bool = BoolCons

instance IsList BoolCons where
  type Item BoolCons = BoolCons
  fromList = foldr (\/) bottom
  toList t = [t]

