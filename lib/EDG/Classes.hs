
module EDG.Classes where

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map


import Algebra.PartialOrd
import Algebra.Lattice


import Data.SBV

-- | Class for elements that can be represented as either a concrete value or
--   a set of constraints on the concrete value.
class ( BoundedJoinSemiLattice (Constraints t)
      , PartialOrd (Constraints t)
      ) => Constrainable t where

  -- | The type of the constraints that can be placed on t, should be a
  --   bounded join semilattice such that:
  --
  --   `bottom` validates with all t
  --   prop>  forall t. validate bottom t == true
  --
  --   Equality over constraints should be identical to equality of the set
  --   of concrete values that sucessfully validate.
  --   prop> forall c1 c2 t. (c1 == c2) => (validate c1 t == validate c2 t)
  --
  --   If a value validates with a constraint, then it should also validate
  --   with a weaker constraint.
  --   prop>  forall c1 c2 t. (validate c1 t) && (c2 `leq` c1) => (validate c2 t)
  --
  --   In addition to satisfying all the laws on bounded semi-lattices and
  --   their corresponding partial orders.
  --
  -- TODO :: Add quickcheck(/SBV?) test generator for constraint properties.
  --
  data Constraints t :: *

  -- | Given a set of constraints on a value, and a value, return whether the
  --   value matches the set of constraints.
  validate :: Constraints t -> t -> Bool

  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  consistent :: Constraints t -> Bool

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  collapse :: Constraints t -> Maybe t


-- | Check whether a set of constraints are non-realizable and that there
--   exist no values that can satisfy them.
inconsistent :: (Constrainable t) => Constraints t -> Bool
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
data Ambiguous t where
  -- | A single concrete value of type t.
  --   "I am <value> of type t"
  Concrete   ::  t -> Ambiguous t
  -- | An abstract set of constraints over elements of type t
  --   "There is a range of values of type t that I could be"
  Abstract   :: (Constrainable t) => (Constraints t) -> Ambiguous t
  -- | An overconstrained value, such that there is not real value that it
  --   could represent.
  --   "I cannot exist"
  Impossible :: Ambiguous t

instance (Eq t) => Eq (Ambiguous t) where
  (==) (Concrete t) (Concrete t') = t == t'
  (==) (Concrete t) (Abstract c') = collapseEq c' t
  (==) (Concrete _) (Impossible ) = False
  (==) (Abstract c) (Concrete t') = collapseEq c  t'
  (==) (Abstract c) (Abstract c') = c == c'
  (==) (Abstract c) (Impossible ) = inconsistent c
  (==) (Impossible) (Concrete _ ) = False
  (==) (Impossible) (Abstract c') = inconsistent c'
  (==) (Impossible) (Impossible ) = True

instance (Eq t) => PartialOrd (Ambiguous t) where
  leq (Concrete t) (Concrete t') = t == t'
  leq (Concrete t) (Abstract c') = collapseEq c' t
  leq (Concrete t) (Impossible ) = True
  leq (Abstract c) (Concrete t') = validate c t'
  leq (Abstract c) (Abstract c') = c `leq` c'
  leq (Abstract c) (Impossible ) = True
  leq (Impossible) (Concrete _ ) = False
  leq (Impossible) (Abstract c') = inconsistent c'
  leq (Impossible) (Impossible ) = True

instance (Eq t) => JoinSemiLattice (Ambiguous t) where
  (\/)   (Impossible)    _             = Impossible
  (\/)   _               (Impossible ) = Impossible
  (\/) a@(Concrete t)    (Concrete t') = if t == t'        then a  else Impossible
  (\/) a@(Concrete t)    (Abstract c') = if validate c' t  then a  else Impossible
  (\/)   (Abstract c) a'@(Concrete t') = if validate c  t' then a' else Impossible
  (\/)   (Abstract c)    (Abstract c')
    | inconsistent c'' = Impossible
    | otherwise        = fromMaybe (Abstract c'') (Concrete <$> collapse c'')
    where c'' = c \/ c'

instance (Eq t, Constrainable t) => BoundedJoinSemiLattice (Ambiguous t) where
  bottom = Abstract bottom

-- ## Typeclasses for various common constraints ##

type IsInclusive = Bool

class OneOfConstraint t where
  oneOf :: (Constrainable t) => [t] -> Constraints t
  is    :: (Constrainable t) =>  t  -> Constraints t

class NoneOfConstraint t where
  noneOf :: (Constrainable t) => [t] -> Constraints t
  isNot  :: (Constrainable t) =>  t  -> Constraints t


class GTConstraint t where
  greaterThan   :: (Constrainable t) => t -> Constraints t
  greaterThanEq :: (Constrainable t) => t -> Constraints t

class LTConstraint t where
  lessThan   :: (Constrainable t) => t -> Constraints t
  lessThanEq :: (Constrainable t) => t -> Constraints t


data IntConstraints = IntConstraints {
    icOneOf       :: [Int]
  , icNoneOf      :: [Int]
  , icLessThan    :: Maybe (Int, IsInclusive)
  , icGreaterThan :: Maybe (Int, IsInclusive)
  }

data AlgRealConstraints = ARConstraints {
    arcOneOf :: [AlgReal]
  , arcNoneOf :: [AlgReal]
  , arcLessThan :: Maybe (AlgReal, IsInclusive)
  , arcGreaterThan :: Maybe (AlgReal, IsInclusive)
  }

data StrConstraints = SConstraints {
    scOneOf :: [String]
  , scNoneOF :: [String]
  }


data UID = UID

data UIDConstraints = UIDConstraints

type FieldName = String

data Field
  = StrField String
  | IntField Int
  | AlgRealField AlgReal
  | UIDField UID
  | RField (Record Field)

data FieldConstraints
  = SFConstraints StrConstraints
  | IFConstraints IntConstraints
  | ARFConstraints AlgRealConstraints
  | UFConstraints UIDConstraints
  | RFConstraints (RecordConstraints Field)

data Record f = Record (Map FieldName f)

data RecordConstraints f = RecContraints {
    rcFieldConstraints :: Map FieldName (Ambiguous f)
  }

-- Int
-- String
-- Field
