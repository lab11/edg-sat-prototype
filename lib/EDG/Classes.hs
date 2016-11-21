
module EDG.Classes where

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.PartialOrd
import Algebra.Lattice

import Text.ParserCombinators.ReadP

import Data.SBV

-- | Class for elements that can be represented as either a concrete value or
--   a set of constraints on the concrete value.
class ( BoundedJoinSemiLattice c
      , PartialOrd c
      ) => Constrainable t c | c -> t where

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

  -- | Given a set of constraints on a value, and a value, return whether the
  --   value matches the set of constraints.
  validate :: c -> t -> Bool

  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  consistent :: c -> Bool

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  collapse :: c -> Maybe t


-- | Check whether a set of constraints are non-realizable and that there
--   exist no values that can satisfy them.
inconsistent :: (Constrainable t c) => c -> Bool
inconsistent = not . consistent

-- | Can a set of constraints be collapsed into a value that's equal to
--   another value that we happen to have on hand?
--
--   TODO :: Make sure this isn't exported from the module it's found in, it's
--           primarily a helper function for the various bollean realtions we
--           have to define.
collapseEq :: (Constrainable t c, Eq t) => c -> t -> Bool
collapseEq c t = fromMaybe False $ (t ==) <$> collapse c

-- | A value that can capture a space of possible values of type t.
data Ambiguous t c where
  -- | A single concrete value of type t.
  --   "I am <value> of type t"
  Concrete   :: t -> Ambiguous t c
  -- | An abstract set of constraints over elements of type t
  --   "There is a range of values of type t that I could be"
  Abstract   :: (Constrainable t c) => c -> Ambiguous t c
  -- | An overconstrained value, such that there is not real value that it
  --   could represent.
  --   "I cannot exist"
  Impossible :: Ambiguous t c

instance (Constrainable t c, Show t, Show c) => Show (Ambiguous t c) where
  showsPrec d (Concrete t)
    = showParen (d > app_prec) $ showString "Concrete " . showsPrec (app_prec + 1) t
      where app_prec = 10
  showsPrec d (Abstract c)
    = showParen (d > app_prec) $ showString "Abstract " . showsPrec (app_prec + 1) c
      where app_prec = 10
  showsPrec d (Impossible)
    = showParen (d > app_prec) $ showString "Impossible"
      where app_prec = 10

instance (Constrainable t c, Read t, Read c) => Read (Ambiguous t c) where
  readsPrec = undefined


instance (Eq t) => Eq (Ambiguous t c) where
  (==) (Concrete t) (Concrete t') = t == t'
  (==) (Concrete t) (Abstract c') = collapseEq c' t
  (==) (Concrete _) (Impossible ) = False
  (==) (Abstract c) (Concrete t') = collapseEq c  t'
  (==) (Abstract c) (Abstract c') = c == c'
  (==) (Abstract c) (Impossible ) = inconsistent c
  (==) (Impossible) (Concrete _ ) = False
  (==) (Impossible) (Abstract c') = inconsistent c'
  (==) (Impossible) (Impossible ) = True

instance (Eq t) => PartialOrd (Ambiguous t c) where
  leq (Concrete t) (Concrete t') = t == t'
  leq (Concrete t) (Abstract c') = collapseEq c' t
  leq (Concrete t) (Impossible ) = True
  leq (Abstract c) (Concrete t') = validate c t'
  leq (Abstract c) (Abstract c') = c `leq` c'
  leq (Abstract c) (Impossible ) = True
  leq (Impossible) (Concrete _ ) = False
  leq (Impossible) (Abstract c') = inconsistent c'
  leq (Impossible) (Impossible ) = True

instance (Eq t) => JoinSemiLattice (Ambiguous t c) where
  (\/)   (Impossible)    _             = Impossible
  (\/)   _               (Impossible ) = Impossible
  (\/) a@(Concrete t)    (Concrete t') = if t == t'        then a  else Impossible
  (\/) a@(Concrete t)    (Abstract c') = if validate c' t  then a  else Impossible
  (\/)   (Abstract c) a'@(Concrete t') = if validate c  t' then a' else Impossible
  (\/)   (Abstract c)    (Abstract c')
    | inconsistent c'' = Impossible
    | otherwise        = fromMaybe (Abstract c'') (Concrete <$> collapse c'')
    where c'' = c \/ c'

instance (Eq t, Constrainable t c) => BoundedJoinSemiLattice (Ambiguous t c) where
  bottom = Abstract bottom

-- ## Typeclasses for various common constraints ##

type IsInclusive = Bool

class OneOfConstraint t where
  oneOf :: (Constrainable t c) => [t] -> c
  is    :: (Constrainable t c) =>  t  -> c

class NoneOfConstraint t where
  noneOf :: (Constrainable t c) => [t] -> c
  isNot  :: (Constrainable t c) =>  t  -> c

class GTConstraint t where
  greaterThan   :: (Constrainable t c) => t -> c
  greaterThanEq :: (Constrainable t c) => t -> c

class LTConstraint t where
  lessThan   :: (Constrainable t c) => t -> c
  lessThanEq :: (Constrainable t c) => t -> c

class UniqConstraint t where
  unique :: (Constrainable t c) => c

data IntConstraints = IntConstraints {
    icOneOf       :: Maybe (Set Int)
  , icNoneOf      :: Maybe (Set Int)
  , icLessThan    :: Maybe (Int, IsInclusive)
  , icGreaterThan :: Maybe (Int, IsInclusive)
  } deriving (Show, Read, Eq)

data FloatConstraints = FloatConstraints {
    fcOneOf       :: Maybe (Set Float)
  , fcNoneOf      :: Maybe (Set Float)
  , fcLessThan    :: Maybe (Float, IsInclusive)
  , fcGreaterThan :: Maybe (Float, IsInclusive)
  } deriving (Show, Read, Eq)

data StrConstraints = SConstraints {
    scOneOf  :: Maybe (Set String)
  , scNoneOF :: Maybe (Set String)
  } deriving (Show, Read, Eq)

data UID = UID Int
  deriving (Show, Read, Eq)

data UIDConstraints = UIDConstraints {
    ucIsNew :: Bool
  } deriving (Show, Read, Eq)

type FieldName = String

data Record f = Record (Map FieldName f)
  deriving (Show, Read, Eq)

data RecordConstraints f c= RecContraints {
    rcFieldConstraints :: Map FieldName (Ambiguous f c)
  } deriving (Show, Read, Eq)

data TypeVal
  = StrVal String
  | IntVal Int
  | FltVal Float
  | UIDVal UID
  | RecVal (Record TypeVal)
  deriving (Show, Read, Eq)

data TypeValConstraints
  = SVConstraints StrConstraints
  | IVConstraints IntConstraints
  | FVConstraints FloatConstraints
  | UVConstraints UIDConstraints
  | RVConstraints (RecordConstraints TypeVal TypeValConstraints)
  | Inconsistent
  deriving (Show, Read, Eq)

instance PartialOrd TypeValConstraints where
  leq = undefined

instance JoinSemiLattice TypeValConstraints where
  (\/) = undefined

instance BoundedJoinSemiLattice TypeValConstraints where
  bottom = undefined

instance Constrainable TypeVal TypeValConstraints where
  validate = undefined
  consistent = undefined
  collapse = undefined
-- Inte
-- String
-- Range t = PartialOrd t =>
--
-- Field


