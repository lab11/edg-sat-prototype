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

-- TODO :: Rewrite all the documentation for this, since it assumes different
--         names on everything

-- | Class for elements that can be represented as either a concrete value or
--   a set of constraints on the concrete value.
class (AsPredicate (Constraints t)
      ,SATAblePredicate (Constraints t)
      ,CollapseablePredicate (Constraints t)
      ,LiftablePredicate (Constraints t)
      ,PredDom (Constraints t) ~ t
      ) => Constrainable t where

  -- | The type of the constraints that can be placed on `t`.
  --
  --   In general it should follow these laws:
  --
  --      If a constraint is invalid it should validate with no t.
  --      prop> forall c t. inconsistent c => not (validate c t)
  --
  --      If a value is lifted into a constraint, then that value should
  --      be a member of the constraint.
  --      prop> forall t. validate (makeConstraint t) t
  --
  --      If a constraint can be collapsed into a value, that is the only
  --      value that validate with the constraint.
  --      prop> forall c t. (collapse c == t) => validate c t
  --      prop> forall c t1 t2. (collapse c == t1) && (t1 /= t2) => not (validate c t2)
  --
  --   If there is a notion of equality over the constraints the following
  --   laws should hold:
  --
  --      Equality over constraints should be identical to equality of the set
  --      of concrete values that sucessfully validate.
  --      prop> forall c1 c2 t. (c1 == c2) => (validate c1 t == validate c2 t)
  --
  --   Any notion of a partial order over the constraints should be consistent
  --   with the notion of equality and also follow these laws
  --
  --      If a value validates with a constraint, then it should also validate
  --      with a weaker constraint. The greater a constraint is, the fewer
  --      values validate under it.
  --      prop>  forall c1 c2 t. (validate c1 t) && (c2 `leq` c1) => (validate c2 t)
  --
  --   Any notion of a semilattice over the constraints should be consistent
  --   with the notion of equality and partial order that was already defined.
  --
  --   Any notion of a bounded join semilattice should follow the following
  --   laws and be consistent with other notions of ordering and lattice ops:
  --
  --      `bottom` validates with all t
  --      prop>  forall t. validate bottom t == true
  --
  --   Any notion of a bounded meet semilattice should follow the following
  --   laws and be consistent with other notions of ordering and lattice ops:
  --
  --      The top of the semilattice is inconsistent.
  --      prop> inconsistent top
  --
  -- TODO :: Add quickcheck(/SBV?) test generator for constraint properties.
  --
  type Constraints t = r | r -> t

  -- | Given a set of constraints on a value, and a value, return whether the
  --   value matches the set of constraints.
  -- validate :: Constraints t -> t -> Bool

  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  -- consistent :: Constraints t -> Bool

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  -- collapse :: Constraints t -> Maybe t

  -- | Attempt to lift a concrete value into a constraint that covers it. This
  --   is used to allow us to more easily define the MeetSemiLattice instances
  --   for an Ambiguous t
  -- makeConstraint :: t -> Constraints t

-- | Check whether a set of constraints are non-realizable and that there
--   exist no values that can satisfy them.
-- inconsistent :: (Constrainable t) => Constraints t -> Bool
-- inconsistent = not . consistent

-- | Can a set of constraints be collapsed into a value that's equal to
--   another value that we happen to have on hand?
--
--   TODO :: Make sure this isn't exported from the module it's found in, it's
--           primarily a helper function for the various bollean realtions we
--           have to define.
-- collapseEq :: (Constrainable t, Eq t) => Constraints t -> t -> Bool
-- collapseEq c t = fromMaybe False $ (t ==) <$> collapse c
--
-- -- | A value that can capture a space of possible values of type t.
-- data Ambiguous t where
--   -- | A single concrete value of type t.
--   --   "I am <value> of type t"
--   Concrete   ::  t -> Ambiguous t
--   -- | An abstract set of constraints over elements of type t
--   --   "There is a range of values of type t that I could be"
--   Abstract   :: (Constrainable t) => (Constraints t) -> Ambiguous t
--   -- | An overconstrained value, such that there is not real value that it
--   --   could represent.
--   --   "I cannot exist"
--   Impossible :: Ambiguous t
--
-- instance (Constrainable t, Show t, Show (Constraints t)) => Show (Ambiguous t) where
--   showsPrec d (Concrete t)
--     = showParen (d > app_prec) $ showString "Concrete " . showsPrec (app_prec + 1) t
--       where app_prec = 10
--   showsPrec d (Abstract c)
--     = showParen (d > app_prec) $ showString "Abstract " . showsPrec (app_prec + 1) c
--       where app_prec = 10
--   showsPrec d (Impossible)
--     = showParen (d > app_prec) $ showString "Impossible"
--       where app_prec = 10
--
-- instance (Constrainable t, Read t, Read (Constraints t)) => Read (Ambiguous t) where
--   readPrec
--     = parens $ (prec app_prec $ do
--                   Ident "Concrete" <- lexP
--                   m <- step readPrec
--                   return (Concrete m))
--            +++ (prec app_prec $ do
--                   Ident "Abstract" <- lexP
--                   m <- step readPrec
--                   return (Abstract m))
--            +++ (prec app_prec $ do
--                   Ident "Impossible" <- lexP
--                   return Impossible)
--       where app_prec = 10
--
-- instance (Constrainable t,Eq t,Eq (Constraints t)) => Eq (Ambiguous t) where
--   (==) (Concrete t) (Concrete t') = t == t'
--   (==) (Concrete t) (Abstract c') = collapseEq c' t
--   (==) (Concrete _) (Impossible ) = False
--   (==) (Abstract c) (Concrete t') = collapseEq c  t'
--   (==) (Abstract c) (Abstract c') = c == c'
--   (==) (Abstract c) (Impossible ) = inconsistent c
--   (==) (Impossible) (Concrete _ ) = False
--   (==) (Impossible) (Abstract c') = inconsistent c'
--   (==) (Impossible) (Impossible ) = True
--
-- instance (Constrainable t,Eq t,PartialOrd (Constraints t)) => PartialOrd (Ambiguous t) where
--   leq (Concrete t) (Concrete t') = t == t'
--   leq (Concrete t) (Abstract c') = collapseEq c' t
--   leq (Concrete t) (Impossible ) = True
--   leq (Abstract c) (Concrete t') = validate c t'
--   leq (Abstract c) (Abstract c') = c `leq` c'
--   leq (Abstract c) (Impossible ) = True
--   leq (Impossible) (Concrete _ ) = False
--   leq (Impossible) (Abstract c') = inconsistent c'
--   leq (Impossible) (Impossible ) = True
--
-- instance (Constrainable t,Eq t,JoinSemiLattice (Constraints t)) => JoinSemiLattice (Ambiguous t) where
--   (\/)   (Impossible)    _             = Impossible
--   (\/)   _               (Impossible ) = Impossible
--   (\/) a@(Concrete t)    (Concrete t') = if t == t'        then a  else Impossible
--   (\/) a@(Concrete t)    (Abstract c') = if validate c' t  then a  else Impossible
--   (\/)   (Abstract c) a'@(Concrete t') = if validate c  t' then a' else Impossible
--   (\/)   (Abstract c)    (Abstract c')
--     | inconsistent c'' = Impossible
--     | otherwise        = fromMaybe (Abstract c'') (Concrete <$> collapse c'')
--     where c'' = c \/ c'
--
-- instance (Constrainable t, Eq t, BoundedJoinSemiLattice (Constraints t)) => BoundedJoinSemiLattice (Ambiguous t) where
--   bottom = Abstract bottom
--
-- -- TODO :: Add instances for `MeetSemiLattice (Ambiguous t)` and
-- --         `BoundedMeetSemiLattice (Ambiguous t)`.
