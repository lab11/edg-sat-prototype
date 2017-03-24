
module EDG.Library.Types.String where

import Algebra.PartialOrd
import Algebra.Lattice
import Algebra.AsPredicate
import Algebra.Constrainable

import GHC.Generics
import Control.DeepSeq

import Control.Newtype
import Control.Newtype.Util

import EDG.Predicates

import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Exts

import Control.Applicative

import GHC.Exts

--
data StringCons
  -- We should never have both an extant OneOf constraint and a NoneOf
  -- constraint, since we can just filter the noneOf elements out of the oneOf
  = SCOneOf  (OneOf String)
  | SCNoneOf (NoneOf String)
  | SCBottom
  deriving (Show, Read, Eq, Generic, NFData)

instance AsPredicate StringCons where
  type PredicateDomain StringCons = String
  asPredicate SCBottom = const True
  asPredicate (SCOneOf a) = asPredicate a
  asPredicate (SCNoneOf a) = asPredicate a

instance SATAblePredicate StringCons where
  isSAT (SCOneOf a) = isSAT a
  isSAT _ = True

instance CollapseablePredicate StringCons where
  collapse (SCOneOf a) = collapse a
  collapse _ = Nothing

instance LiftablePredicate StringCons where
  liftPredicate = SCOneOf . liftPredicate

instance BottomPredicate StringCons where
  isBottom SCBottom = True
  isBottom _ = False

instance PartialOrd StringCons where
  leq (SCOneOf  n) (SCOneOf  n') = n `leq` n'
  leq (SCNoneOf n) (SCNoneOf n') = n `leq` n'
  leq (SCOneOf  n) (SCNoneOf n') = n `leqOneNone` n'
  leq (SCNoneOf n) (SCOneOf  n') = n `leqNoneOne` n'
  leq (SCBottom  ) _             = True
  leq _            (SCBottom   ) = False

instance JoinSemiLattice StringCons where
  (\/) (SCBottom  ) a             = a
  (\/) a            (SCBottom   ) = a
  (\/) (SCOneOf  n) (SCOneOf  n') = SCOneOf  $ n \/ n'
  (\/) (SCNoneOf n) (SCNoneOf n') = SCNoneOf $ n \/ n'
  (\/) (SCOneOf  n) (SCNoneOf n') = SCOneOf $ joinOneOf n' n
  (\/) (SCNoneOf n) (SCOneOf  n') = SCOneOf $ joinOneOf n  n'

instance MeetSemiLattice StringCons where
  (/\) (SCBottom  ) _             = SCBottom
  (/\) _            (SCBottom   ) = SCBottom
  (/\) (SCOneOf  n) (SCOneOf  n') = SCOneOf  $ n /\ n'
  (/\) (SCNoneOf n) (SCNoneOf n') = SCNoneOf $ n /\ n'
  (/\) (SCOneOf  n) (SCNoneOf n') = SCNoneOf $ meetNoneOf n  n'
  (/\) (SCNoneOf n) (SCOneOf  n') = SCNoneOf $ meetNoneOf n' n

instance BoundedJoinSemiLattice StringCons where
  bottom = SCBottom

instance BoundedMeetSemiLattice StringCons where
  top = SCOneOf top

instance OneOfConstraint StringCons where
  oneOf l = SCOneOf $ oneOf l

instance NoneOfConstraint StringCons where
  noneOf l = SCNoneOf $ noneOf l

instance Constrainable String where
  type Constraints String = StringCons

instance IsList StringCons where
  type Item StringCons = StringCons
  fromList = foldr (\/) bottom
  toList t = [t]

