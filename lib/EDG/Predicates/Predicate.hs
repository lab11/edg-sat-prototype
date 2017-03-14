
module EDG.Predicates.Predicate where

import Algebra.Lattice
import Algebra.AsPredicate

import Control.Newtype
import Control.Newtype.Util

-- | Wrapper type for
newtype Predicate a = Predicate (a -> Bool)

instance Newtype (Predicate a) (a -> Bool) where
  pack = Predicate
  unpack (Predicate a) = a

-- | The simplest instance of `AsPredicate`, predicates. We can't do much with
--   this but it's incredibly useful for instances where we want to define
instance AsPredicate (Predicate a) where
  type PredicateDomain (Predicate a) = a
  asPredicate = unpack

instance (Eq a) => LiftablePredicate (Predicate a) where
  liftPredicate a = pack (\ v -> v == a)

-- | Make a `Predicate` out of something that can be used AsPredicate
makePredicate :: AsPredicate a => a -> Predicate (PredDom a)
makePredicate = pack . asPredicate

-- We can't actually define instances of Eq, PartialOrd, SATAblePredicate, or
-- CollapseablePredicate for this type but they would work the expected way.
-- We just can't introspect into the function enough and the domain may not
-- small enough to feasibly check over.
--
-- > a == b if values satisfy a if and only if they satisfy b and vice versa
-- > a <= b if every value that satisfies b also satisfies a
--
-- As such, these instances mostly exist for testing and to help provide a more
-- useful definition for other predicates.

-- | The join of two predicates is the predicate that's true when both input
--   predicates are true.
instance JoinSemiLattice (Predicate a) where
  (\/) (Predicate a) (Predicate b) = Predicate $ (&&) <$> a <*> b

-- | The meet of two predicates is true when either predicate is true.
instance MeetSemiLattice (Predicate a) where
  (/\) (Predicate a) (Predicate b) = Predicate $ (||) <$> a <*> b

-- | The bottom element of a predicate a just always returns True, allowing
--   any element.
instance BoundedJoinSemiLattice (Predicate a) where
  bottom = Predicate $ const True

-- | The top element of the Predicate Lattice always returns False, meaning
--   that nothing passes it.
instance BoundedMeetSemiLattice (Predicate a) where
  top = Predicate $ const False

instance Lattice (Predicate a)
instance BoundedLattice (Predicate a)
