
{-# LANGUAGE UndecidableInstances #-}

-- | This module defined both the typeVal that encapsulates all the other
--   partial types we've got handy, as well as the kindVal that will be useful
--   when we're working with lots of random types and want to make them look
--   nicer.
module EDG.Library.Types.TypeVal where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Fix -- newtype Fix f = Fix {unFix :: f (Fix f)}

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate
import Algebra.Constrainable

import EDG.Predicates

import EDG.Classes.Normalizable

import Algebra.Lattice
import Algebra.PartialOrd

import Control.Newtype
import Control.Newtype.Util

import Control.Applicative

import GHC.Exts

-- The Subtypes we're going to be working with.
import EDG.Library.Types.Bool
import EDG.Library.Types.Float
import EDG.Library.Types.Integer
import EDG.Library.Types.String
import EDG.Library.Types.UID
import EDG.Library.Types.Record

-- | This is the basic TypeVal', which disambiguates between each of the types
--   we have handy, each constructor can also act as a value level witness if
--   we need one.
--
--   Like the record, this is parameterized so that we can get the fixed point
--   in EDG.Library.Types where all the neccesary information is lying around
--   in one place.
--
data TypeVal' a where
  TVBool   :: Bool        -> TypeVal' a
  TVFloat  :: Float       -> TypeVal' a
  TVInt    :: Integer     -> TypeVal' a
  TVString :: String      -> TypeVal' a
  TVUID    :: UID Integer -> TypeVal' a
  TVRecord :: Record' a   -> TypeVal' a
  deriving (Show,Read,Eq)

-- | This is the type alias that allows the TypeVal' to recurse as deep as
--   needed, with records all the way down.
type TypeVal = Fix TypeVal'

-- | Convinience alias for the type of record that most people will actually
--   be working with (Yes, there's a lot of extra `Fix` es whenever you try
--   to pattern match.)
type Record = Record' TypeVal

-- | And this is the corresponding type for the constraints over a TypedValue.
--   Same deal with the parameterization.
--
--   I'm getting the constraints here rather that just typing it out because
--   it prevents user error if we change the type of some constraint.
--
--   TODO :: Feh, I'm seriously hating having to put all these explicit bottoms
--           and tops in all of our constraint dataTypes, they
data TypeCons' a where
  TCBool   :: (Constraints Bool         ) -> TypeCons' a
  TCFloat  :: (Constraints Float        ) -> TypeCons' a
  TCInt    :: (Constraints Integer      ) -> TypeCons' a
  TCString :: (Constraints String       ) -> TypeCons' a
  TCUID    :: (Constraints (UID Integer)) -> TypeCons' a
  TCRecord :: (Constraints (Record' a  )) -> TypeCons' a
  TCBottom :: TypeCons' a
  TCTop    :: TypeCons' a


-- | Remember, A TypeCons' uses the same parameter as the TypeVal' it's using.
--   The chain of Ambiguous and Constrainable values in there actually unwrap
--   into the structure we want, we just have some extra Fix values lying
--   around.
--
--   The newtype here is nessesary to preserve the injectivity of the
--   Constrainable typeclass. Otherwise it's just identical to the standard
--   constructor you'd expect.
--
--   Most of the instances just unwrap and wrap this newtype and the Fix
--   around the TypeVal'. Don't worry about it too much, in practice it should
--   be pretty invisible.
newtype TypeCons = TypeCons (TypeCons' TypeVal)
  deriving (Eq,Show,Read)

instance Newtype TypeCons (TypeCons' TypeVal) where
  pack = TypeCons
  unpack (TypeCons c) = c

-- Again, this is where we need UndecidableInstances, because the
-- `Constraints t` is no smaller than `TypeCons' t`, but the cycle will
-- terminate once you hit the instance for the fixed point operator, since
-- that won't introduce any additional constraints.
deriving instance (Read (f (Fix f))) => Read (Fix f)
deriving instance (      Constrainable t, Eq   t,Eq   (Constraints t)) => Eq   (TypeCons' t)
deriving instance (Eq t, Constrainable t, Show t,Show (Constraints t)) => Show (TypeCons' t)
deriving instance (Eq t, Constrainable t, Read t,Read (Constraints t)) => Read (TypeCons' t)


instance AsPredicate (TypeCons' a) where
  type PredicateDomain (TypeCons' a) = TypeVal' a

  asPredicate (TCBool   c) (TVBool   b) = asPredicate c b
  asPredicate (TCFloat  c) (TVFloat  b) = asPredicate c b
  asPredicate (TCInt    c) (TVInt    b) = asPredicate c b
  asPredicate (TCString c) (TVString b) = asPredicate c b
  asPredicate (TCUID    c) (TVUID    b) = asPredicate c b
  asPredicate (TCRecord c) (TVRecord b) = asPredicate c b
  asPredicate (TCBottom  ) _            = True
  asPredicate _            _            = False

instance AsPredicate TypeCons where
  type PredicateDomain TypeCons = TypeVal

  asPredicate (TypeCons c) (Fix v) = asPredicate c v

instance SATAblePredicate (TypeCons' a) where
  isSAT (TCBool   c) = isSAT c
  isSAT (TCFloat  c) = isSAT c
  isSAT (TCInt    c) = isSAT c
  isSAT (TCString c) = isSAT c
  isSAT (TCUID    c) = isSAT c
  isSAT (TCRecord c) = isSAT c
  isSAT TCBottom = True
  isSAT TCTop = False

instance SATAblePredicate TypeCons where
  isSAT (TypeCons c) = isSAT c

instance BottomPredicate (TypeCons' a) where
  isBottom (TCBool   c) = isBottom c
  isBottom (TCFloat  c) = isBottom c
  isBottom (TCInt    c) = isBottom c
  isBottom (TCString c) = isBottom c
  isBottom (TCUID    c) = isBottom c
  isBottom (TCRecord c) = isBottom c
  isBottom TCBottom = True
  isBottom TCTop = False

instance BottomPredicate TypeCons where
  isBottom (TypeCons c) = isBottom c

instance CollapseablePredicate (TypeCons' a) where
  collapse (TCBool   c) = TVBool   <$> collapse c
  collapse (TCFloat  c) = TVFloat  <$> collapse c
  collapse (TCInt    c) = TVInt    <$> collapse c
  collapse (TCString c) = TVString <$> collapse c
  collapse (TCUID    c) = TVUID    <$> collapse c
  collapse (TCRecord c) = TVRecord <$> collapse c
  collapse _ = Nothing

instance CollapseablePredicate TypeCons where
  collapse (TypeCons c) = Fix <$> collapse c


instance (Eq a,Constrainable a) =>  LiftablePredicate (TypeCons' a) where
  liftPredicate (TVBool   b) = TCBool   $ liftPredicate b
  liftPredicate (TVFloat  b) = TCFloat  $ liftPredicate b
  liftPredicate (TVInt    b) = TCInt    $ liftPredicate b
  liftPredicate (TVString b) = TCString $ liftPredicate b
  liftPredicate (TVUID    b) = TCUID    $ liftPredicate b
  liftPredicate (TVRecord b) = TCRecord $ liftPredicate b

instance LiftablePredicate TypeCons where
  liftPredicate (Fix v) = TypeCons $ liftPredicate v

instance (Eq a,Constrainable a) => Constrainable (TypeVal' a) where
  type Constraints (TypeVal' a) = TypeCons' a

instance Constrainable TypeVal where
  type Constraints TypeVal = TypeCons

instance (Eq a,Constrainable a, PartialOrd (Constraints a)) => PartialOrd (TypeCons' a) where
  leq TCBottom _ = True
  leq _ TCBottom = False
  leq _ TCTop = True
  leq TCTop _ = False
  leq (TCBool   c) (TCBool   c') = c `leq` c'
  leq (TCFloat  c) (TCFloat  c') = c `leq` c'
  leq (TCInt    c) (TCInt    c') = c `leq` c'
  leq (TCString c) (TCString c') = c `leq` c'
  leq (TCUID    c) (TCUID    c') = c `leq` c'
  leq (TCRecord c) (TCRecord c') = c `leq` c'
  leq _ _ = False

instance PartialOrd TypeCons where
  leq (TypeCons c) (TypeCons c') = c `leq` c

instance (Eq a,Constrainable a, JoinSemiLattice (Ambiguous a)) => JoinSemiLattice (TypeCons' a) where
  (\/) TCTop _ = TCTop
  (\/) _ TCTop = TCTop
  (\/) TCBottom a = a
  (\/) a TCBottom = a
  (\/) (TCBool   c) (TCBool   c') = TCBool   $ c \/ c'
  (\/) (TCFloat  c) (TCFloat  c') = TCFloat  $ c \/ c'
  (\/) (TCInt    c) (TCInt    c') = TCInt    $ c \/ c'
  (\/) (TCString c) (TCString c') = TCString $ c \/ c'
  (\/) (TCUID    c) (TCUID    c') = TCUID    $ c \/ c'
  (\/) (TCRecord c) (TCRecord c') = TCRecord $ c \/ c'
  (\/) _ _ = TCTop

instance JoinSemiLattice TypeCons where
  (\/) (TypeCons c) (TypeCons c') = TypeCons $ c \/ c'

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (Eq a,Constrainable a, MeetSemiLattice (Ambiguous a)) => MeetSemiLattice (TypeCons' a) where
  (/\) TCBottom _ = TCBottom
  (/\) _ TCBottom = TCBottom
  (/\) a TCTop = a
  (/\) TCTop a = a
  (/\) (TCBool   c) (TCBool   c') = TCBool   $ c /\ c'
  (/\) (TCFloat  c) (TCFloat  c') = TCFloat  $ c /\ c'
  (/\) (TCInt    c) (TCInt    c') = TCInt    $ c /\ c'
  (/\) (TCString c) (TCString c') = TCString $ c /\ c'
  (/\) (TCUID    c) (TCUID    c') = TCUID    $ c /\ c'
  (/\) (TCRecord c) (TCRecord c') = TCRecord $ c /\ c'
  (/\) _ _ = TCBottom

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (MeetSemiLattice (Ambiguous TypeVal)) => MeetSemiLattice TypeCons where
  (/\) (TypeCons c) (TypeCons c') = TypeCons $ c /\ c'

instance (Eq a,Constrainable a,JoinSemiLattice (Ambiguous a)) => BoundedJoinSemiLattice (TypeCons' a) where
  bottom = TCBottom

instance BoundedJoinSemiLattice TypeCons where
  bottom = TypeCons $ bottom

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (Eq a,Constrainable a,MeetSemiLattice (Ambiguous a)) => BoundedMeetSemiLattice (TypeCons' a) where
  top = TCTop

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (MeetSemiLattice (Ambiguous TypeVal)) => BoundedMeetSemiLattice TypeCons where
  top = TypeCons $ top

-- | This is where we start doing more interesting things with our RecordCons
--   and all the nested types we've got lying around. This lets us present
--   a much nicer interface to the user while preserving all the power in the
--   backend.
instance IsList (RecordCons TypeVal) where
  type Item (RecordCons TypeVal) = (String,Ambiguous TypeVal)

  fromList = RCAmbig . Map.fromList
  toList = error "Don't use list syntax in a pattern match"

-- TODO :: Also create the IsList instance for a normal Record which should
--         just unwrap all the internal values to get a concrete record.

-- | This typeclass allows various other types to be turned into nice record
--   field tuples without all that much effort.
--
--   We need a class here, because it allows us to specify which TypeVal or
--   TypeCons constructor we use at any given point
--   .
class (Constrainable t) => ToRecordField t where
  -- | Write a single concrete value into a record field as follows
  --
  --   > "foo" <: @Integer 4
  --
  -- TODO :: *sigh* This doesn't work as intended because TypeApplication
  --         doesn't play nice with infix operators. Find a nicer syntax
  --         that's actually doable.
  newCField :: String -> t -> (String,Ambiguous TypeVal)
  -- | Write an abstract value into a record field as follows
  --
  --   > "Foo" <~ @Integer [lessThan 3, greaterThanEq -4, oneOf [1,3,4]]
  --
  -- TODO :: *sigh* This doesn't work as intended because TypeApplication
  --         doesn't play nice with infix operators. Find a nicer syntax
  --         that's actually doable.
  newAField :: String -> Constraints t -> (String,Ambiguous TypeVal)

instance ToRecordField Bool where
  newCField s v = (s,Concrete . Fix      . TVBool $ v)
  newAField s c = (s,Abstract . TypeCons . TCBool $ c)

instance ToRecordField Float where
  newCField s v = (s,Concrete . Fix      . TVFloat $ v)
  newAField s c = (s,Abstract . TypeCons . TCFloat $ c)

instance ToRecordField Integer where
  newCField s v = (s,Concrete . Fix      . TVInt $ v)
  newAField s c = (s,Abstract . TypeCons . TCInt $ c)

instance ToRecordField String where
  newCField s v = (s,Concrete . Fix      . TVString $ v)
  newAField s c = (s,Abstract . TypeCons . TCString $ c)

instance ToRecordField (UID Integer) where
  newCField s v = (s,Concrete . Fix      . TVUID $ v)
  newAField s c = (s,Abstract . TypeCons . TCUID $ c)

instance ToRecordField Record where
  newCField s v = (s,Concrete . Fix      . TVRecord $ v)
  newAField s c = (s,Abstract . TypeCons . TCRecord $ c)

(<:-) s c v = (s,Concrete . Fix      . c $ v)


-- | Temporary function for creating a concrete field
(<:) :: ToRecordField t => String -> t -> (String,Ambiguous TypeVal)
(<:) = newCField

-- | Temporary function to create an ambiguous field.
(<~) :: ToRecordField t => String -> Constraints t -> (String,Ambiguous TypeVal)
(<~) = newAField

-- TODO :: Remove this, just testing some stuff.
foo :: RecordCons TypeVal
foo = ["Name" <: @Integer $ 3]

-- -- | Same Deal with kinds, we can    tie the knot externally, and get something
-- --   more useful than just this s   imple witness for the kinds of the first
-- --   set of types.
-- data KindVal' a where
--   KVBool   :: KindVal' a
--   KVFloat  :: KindVal' a
--   KVInt    :: KindVal' a
--   KVString :: KindVal' a
--   KVUID    :: KindVal' a
--   KVRecord :: Map String a -> KindVal' a
--   deriving (Show,Read,Eq)
--
-- type KindVal = Fix KindVal'
