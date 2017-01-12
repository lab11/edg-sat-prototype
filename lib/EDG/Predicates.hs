
module EDG.Predicates
  ( module EDG.Predicates.Bounds
  , module EDG.Predicates.Maybe
  , module EDG.Predicates.OneOf
  , module EDG.Predicates.NoneOf
  , module EDG.Predicates.Predicate
  , module EDG.Predicates.Range
  , module EDG.Predicates.Tuple
) where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

import EDG.Predicates.Bounds
import EDG.Predicates.Maybe
import EDG.Predicates.OneOf
import EDG.Predicates.NoneOf
import EDG.Predicates.Predicate
import EDG.Predicates.Range
import EDG.Predicates.Tuple

-- This module is meant to contain all the cross predicate interactions as well
-- as re-exporting each induvidual predicate we happen to be using,

-- TODO: No Idea what to do with this right now
-- class UniqConstraint t where
--   unique :: (Constrainable t) => t -> Constraints t
--
-- Do we need a (Unique) instance for AsPredicate? How would that work?
--


