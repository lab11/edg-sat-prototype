
module EDG.Predicates.Maybe where

import Algebra.PartialOrd
import Algebra.Lattice
import Algebra.AsPredicate

import EDG.Predicates.Bounds
import EDG.Predicates.OneOf
import EDG.Predicates.NoneOf

-- | A predicate with a maybe wrapped around it, where Just is just the
--   predicate and Nothing is bottom.
instance (AsPredicate a) => AsPredicate (Maybe a) where
  type PredicateDomain (Maybe a) = (PredDom a)
  asPredicate Nothing  = const True
  asPredicate (Just a) = asPredicate a

instance (AsPredicate a,SATAblePredicate a) => SATAblePredicate (Maybe a) where
  isSAT Nothing = True
  isSAT (Just a) = isSAT a

instance (AsPredicate a,CollapseablePredicate a) => CollapseablePredicate (Maybe a) where
  collapse Nothing = Nothing
  collapse (Just a) = collapse a

instance (AsPredicate a,LiftablePredicate a) => LiftablePredicate (Maybe a) where
  liftPredicate a = Just $ liftPredicate a

instance (AsPredicate a, BottomPredicate a) => BottomPredicate (Maybe a) where
  isBottom (Just a) = isBottom a
  isBottom Nothing = True

instance (AsPredicate a,PartialOrd a) => PartialOrd (Maybe a) where
  leq Nothing _ = True
  leq _ Nothing = False
  leq (Just a) (Just b) = a `leq` b

instance (AsPredicate a,JoinSemiLattice a) => JoinSemiLattice (Maybe a) where
  (\/) Nothing a = a
  (\/) a Nothing = a
  (\/) (Just a) (Just b) = Just $ a \/ b

instance (AsPredicate a,MeetSemiLattice a) => MeetSemiLattice (Maybe a) where
  (/\) Nothing _ = Nothing
  (/\) _ Nothing = Nothing
  (/\) (Just a) (Just b) = Just $ a /\ b

instance (AsPredicate a, BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Maybe a) where
  top = Just top

instance (AsPredicate a,JoinSemiLattice a) => BoundedJoinSemiLattice (Maybe a) where
  bottom = Nothing

instance (OneOfConstraint t) => OneOfConstraint (Maybe t) where
  oneOf l = Just $ oneOf l

instance (NoneOfConstraint t) => NoneOfConstraint (Maybe t) where
  noneOf l = Just $ noneOf l

instance (GTConstraint t) => GTConstraint (Maybe t) where
  greaterThan   v = Just $ greaterThan v
  greaterThanEq v = Just $ greaterThanEq v

instance (LTConstraint t) => LTConstraint (Maybe t) where
  lessThan   v = Just $ lessThan v
  lessThanEq v = Just $ lessThanEq v

