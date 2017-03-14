
module Algebra.AsPredicate where

-- | Synonyms to minimize typing
type PredDom a  = PredicateDomain a
type AsPred a   = AsPredicate a
type SATPred a  = SATAblePredicate a
type ColPred a  = CollapseablePredicate a
type LiftPred a = LiftablePredicate a

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
  --
  --   basically does this predicate cover top?
  collapse :: a -> Maybe (PredDom a)

class AsPredicate a => LiftablePredicate a where
  -- | Given a single value of type `a` return a perdicate that is true for
  --   only that value.
  liftPredicate :: PredDom a -> a

-- | If you either a predicate or a value in the predicate's domain, turn it
--   into a predicate as needed.
liftEither :: (LiftablePredicate a) => Either a (PredDom a) -> a
liftEither (Left  a) = a
liftEither (Right v) = liftPredicate v

class AsPredicate a => BottomPredicate a where
  -- | Is the predicate true for all inputs? is is bottom?
  isBottom :: a -> Bool
