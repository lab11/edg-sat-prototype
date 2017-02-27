{-# LANGUAGE UndecidableInstances #-}

-- Most of the classes and derived instances here need this flag because we
-- want to talk about the instances of the constraints for a type.
--
-- Without it we couldn't have any contraints that look like
--
-- `(Constrainable t, <Some Class> (Constraint t)) => <Some Type Involving t>`
--
-- This should terminate because `Constraint t` is defined as a data family with
-- kind `*`. `t` should both uniquely determine `Constraint t` and never be
-- equal to it.
--
-- Of course, this breaks if you're using Undecidable instances elsewhere in
-- a different manner.
--
-- TODO :: This needs an actual proof, rather than my vague sketch.

module Algebra.Constrainable where

import Data.Maybe (fromMaybe)

import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.AsPredicate

import Text.Read

-- | Class for elements that can be represented as either a concrete value or
--   a set of constraints on the concrete value.
class (AsPredicate           (Constraints t)
      ,SATAblePredicate      (Constraints t)
      ,CollapseablePredicate (Constraints t)
      ,LiftablePredicate     (Constraints t)
      ,BottomPredicate       (Constraints t)
      ,PredDom (Constraints t) ~ t
      ) => Constrainable t where

  -- | The type of the canonical constraints that can be placed on `t`.
  --   This is an injective type family that we can use to set up defaults for
  --   each value element.
  type Constraints t = r | r -> t

-- | Given a set of constraints on a value, and a value, return whether the
--   value matches the set of constraints.
validate :: (Constrainable t) => Constraints t -> t -> Bool
validate = asPredicate

-- | Given a set of constraints, check whether the constraints are realizable
--   and there might exist a concrete element of that type.
--
--   This gets called often as part of the normalization procedure of an
--   ambiguous value and any implementation should be fast.
consistent :: Constrainable t => Constraints t -> Bool
consistent = isSAT

-- | Attempt to lift a concrete value into a constraint that covers it. This
--   is used to allow us to more easily define the MeetSemiLattice instances
--   for an Ambiguous t
makeConstraint :: Constrainable t => t -> Constraints t
makeConstraint = liftPredicate

-- | Check whether a set of constraints are non-realizable and that there
--   exist no values that can satisfy them.
inconsistent :: Constrainable t => Constraints t -> Bool
inconsistent = not . consistent

-- | Can a set of constraints be collapsed into a value that's equal to
--   another value that we happen to have on hand?
--
--   TODO :: Make sure this isn't exported from the module it's found in, it's
--           primarily a helper function for the various bollean realtions we
--           have to define.
collapseEq :: (Constrainable t, Eq t) => Constraints t -> t -> Bool
collapseEq c t = fromMaybe False $ (t ==) <$> collapse c

-- | A value that can capture a space of possible values of type t.
--
--   TODO :: We might need a separate value for bottom in here, but this should
--           let us wrap all our ambiguity in a single type now.
data Ambiguous t where
  -- | A single concrete value of type t.
  --   "I am <value> of type t"
  Concrete   :: {value :: t} -> Ambiguous t
  -- | An abstract set of constraints over elements of type t
  --   "There is a range of values of type t that I could be"
  Abstract   :: (Constrainable t) => {constraints :: Constraints t} -> Ambiguous t
  -- | An overconstrained value, such that there is not real value that it
  --   could represent.
  --   "I cannot exist"
  Impossible :: Ambiguous t

-- | Property preserving transformations of ambiguous values.
transformAmbig :: (Constrainable t,Constrainable t')
               => (t -> t') -> (Constraints t -> Constraints t')
               -> Ambiguous t -> Ambiguous t'
transformAmbig _ _  Impossible  = Impossible
transformAmbig f _ (Concrete v) = Concrete (f v)
transformAmbig _ f (Abstract c) = Abstract (f c)

-- | If the value is collapsible or unsatisfiable reduce this to its simplest
--   representation.
flattenAmbig :: (Constrainable t) => Ambiguous t -> Ambiguous t
flattenAmbig a@(Abstract c) | Just v <- collapse c = Concrete v
                            | True  <- unSAT c    = Impossible
                            | otherwise            = a
flattenAmbig a = a

-- | Show instance for Ambiguous values
instance (Constrainable t, Show t, Show (Constraints t)) => Show (Ambiguous t) where
  showsPrec d (Concrete t)
    = showParen (d > app_prec) $ showString "Concrete " . showsPrec (app_prec + 1) t
      where app_prec = 10
  showsPrec d (Abstract c)
    = showParen (d > app_prec) $ showString "Abstract " . showsPrec (app_prec + 1) c
      where app_prec = 10
  showsPrec d (Impossible)
    = showParen (d > app_prec) $ showString "Impossible"
      where app_prec = 10

-- | Read instance for ambiguous values
instance (Constrainable t, Read t, Read (Constraints t)) => Read (Ambiguous t) where
  readPrec
    = parens $ (prec app_prec $ do
                  Ident "Concrete" <- lexP
                  m <- step readPrec
                  return (Concrete m))
           +++ (prec app_prec $ do
                  Ident "Abstract" <- lexP
                  m <- step readPrec
                  return (Abstract m))
           +++ (prec app_prec $ do
                  Ident "Impossible" <- lexP
                  return Impossible)
      where app_prec = 10

instance (Constrainable t,Eq t,Eq (Constraints t)) => Eq (Ambiguous t) where
  (==) (Concrete t) (Concrete t') = t == t'
  (==) (Concrete t) (Abstract c') = collapseEq c' t
  (==) (Concrete _) (Impossible ) = False
  (==) (Abstract c) (Concrete t') = collapseEq c  t'
  (==) (Abstract c) (Abstract c') = c == c'
  (==) (Abstract c) (Impossible ) = inconsistent c
  (==) (Impossible) (Concrete _ ) = False
  (==) (Impossible) (Abstract c') = inconsistent c'
  (==) (Impossible) (Impossible ) = True

instance (Constrainable t,Eq t,PartialOrd (Constraints t)) => PartialOrd (Ambiguous t) where
  leq (Concrete t) (Concrete t') = t == t'
  leq (Concrete t) (Abstract c') = collapseEq c' t
  leq (Concrete t) (Impossible ) = True
  leq (Abstract c) (Concrete t') = validate c t'
  leq (Abstract c) (Abstract c') = c `leq` c'
  leq (Abstract c) (Impossible ) = True
  leq (Impossible) (Concrete _ ) = False
  leq (Impossible) (Abstract c') = inconsistent c'
  leq (Impossible) (Impossible ) = True

instance (Constrainable t,Eq t,JoinSemiLattice (Constraints t)) => JoinSemiLattice (Ambiguous t) where
  (\/)   (Impossible)    _             = Impossible
  (\/)   _               (Impossible ) = Impossible
  (\/) a@(Concrete t)    (Concrete t') = if t == t'        then a  else Impossible
  (\/) a@(Concrete t)    (Abstract c') = if validate c' t  then a  else Impossible
  (\/)   (Abstract c) a'@(Concrete t') = if validate c  t' then a' else Impossible
  (\/)   (Abstract c)    (Abstract c')
    | inconsistent c'' = Impossible
    | otherwise        = fromMaybe (Abstract c'') (Concrete <$> collapse c'')
    where c'' = c \/ c'

instance (Constrainable t, Eq t, BoundedJoinSemiLattice (Constraints t))
  => BoundedJoinSemiLattice (Ambiguous t) where
  bottom = Abstract bottom

instance (Eq t,Constrainable t) => AsPredicate (Ambiguous t) where
  type PredicateDomain (Ambiguous t) = t
  asPredicate (Impossible) = const False
  asPredicate (Concrete t) = (== t)
  asPredicate (Abstract c) = asPredicate c

instance (Eq t, Constrainable t) => SATAblePredicate (Ambiguous t) where
  isSAT (Impossible) = False
  isSAT (Concrete _) = True
  isSAT (Abstract c) = isSAT c

instance (Eq t, Constrainable t) => CollapseablePredicate (Ambiguous t) where
  collapse (Impossible) = Nothing
  collapse (Concrete t) = Just t
  collapse (Abstract c) = collapse c

instance (Eq t,Constrainable t) => LiftablePredicate (Ambiguous t) where
  -- | Given a single value of type `a` return a perdicate that is true for
  --   only that value.
  liftPredicate = Concrete

instance (Eq t, Constrainable t) => BottomPredicate (Ambiguous t) where
  isBottom Impossible   = False
  isBottom (Concrete _) = False
  isBottom (Abstract c) = isBottom c


-- TODO :: Add instances for `MeetSemiLattice (Ambiguous t)` and
--         `BoundedMeetSemiLattice (Ambiguous t)`.
