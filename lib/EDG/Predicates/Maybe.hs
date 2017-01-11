
module EDG.Predicates.Maybe where

import Algebra.PartialOrd
import Algebra.Lattice
import Algebra.AsPredicate

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
