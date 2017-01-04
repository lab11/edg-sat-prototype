
module EDG.Predicates.Maybe where

import Algebra.Lattice
import Algebra.AsPredicate

-- | A predicate with a maybe wrapped around it, where Just is just the
--   predicate and Nothing is bottom.

instance (AsPredicate a) => AsPredicate (Maybe a) where
  type PredicateDomain (Maybe a) = (PredDom a)
  asPredicate Nothing  = const True
  asPredicate (Just a) = asPredicate a
