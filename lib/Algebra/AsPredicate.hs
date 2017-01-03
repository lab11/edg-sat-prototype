
module Algebra.AsPredicate where

-- | Synonym to minimize typing
type PredDom a = PredicateDomain a

-- | Treat a value of some type as a predicate over values of another type.
--   it's pretty convinient.
class AsPredicate a where
  type PredicateDomain a
  -- | Convert an element of type `a` into a predicate over `PredDom a`
  asPredicate :: a -> PredDom a -> Bool

-- | Can you tell whether a particular predicate is satisfiable?
class AsPredicate a => SATAblePredicate a where
  -- | For predicate `a` is there an element `e` in `PredDom a` such that
  --   `asPredicate a e == True`? Should return `True` if it's not known.
  isSAT :: a -> Bool

-- | Is a predicate unsatisfiable?
unSAT :: SATAblePredicate a => a -> Bool
unSAT = not . isSAT

class AsPredicate a => CollapseablePredicate a where
  -- | Given some predicate `a` is there one and only one `e` which will satify
  --   it? if so return Just that element, otherwise return Nothing.
  collapse :: a -> Maybe (PredDom a)


