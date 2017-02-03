
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

import Data.Functor.Identity

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
--
--   TODO :: Make sure there's no sensible list interface for these things, at
--           least insofar as pattern matching is concerned.
--
instance IsList (RecordCons TypeVal) where
  type Item (RecordCons TypeVal) = (String,Ambiguous TypeVal)

  fromList = RCAmbig . Map.fromList
  toList = error "Don't use list syntax in a pattern match"

-- | This instance allows us to make sure that a single static record is
--   actually treated as one, and collapsed into a single value. Problem is
--   that it produces a runtime error rather than a compile time error.
--
--   TODO :: See if you can't get this to produce compile time errors, rather
--           than a runtime error. There should be some typeclass trickery
--           that allows you to use the same operator in multiple contexts.
--
instance IsList Record where
  type Item Record = (String,Ambiguous TypeVal)

  fromList l = case collapse . RCAmbig . Map.fromList $ l of
    Nothing -> error $ "This record isn't a single fixed value :" ++ show l
    Just v  -> v
  toList = error "Don't use list syntax in a pattern match"


-- | This operator allows you to create a record field with a fixed concrete
--   value. Each field is defined as:
--
--      ` <Name of Field> <:= <TV Constructor for field type> $ <field value>`
--   or `(<Name of Field> <:= <TV Constructor for field type>)  <field value>`
--
--   Note :: Yes I'm using the `$` operator to get vaguely mixfix notation for
--           this, it's the best I can do at the moment for vaguely readable
--           easy to write syntax.
--
--  TODO :: There should be some typeclass trickery that allows the use of the
--          same operator and type annotations for both ambiguous and
--          unambiguous records, see if it's a good idea to implement it when
--          you have time.
--
--  TODO :: Add a normal function version of the operator
--
(<:=) :: String -> (t -> TypeVal' TypeVal) -> t -> (String,Ambiguous TypeVal)
(<:=) s c v = (s,Concrete . Fix      . c $ v)


-- | This operator allows you define an ambiguous field, with a series of
--   constraints on it. Each is defined as:
--
--      ` <Name of Field> <~= <TC Constructor for field type> $ <Constraint over field>`
--   or `(<Name of Field> <~= <TC Constructor for field type>)  <Constraint over field>`
(<~=) :: String -> (Constraints t -> TypeCons' TypeVal) -> Constraints t -> (String,Ambiguous TypeVal)
(<~=) s c v = (s,Abstract . TypeCons . c $ v)

-- This is just an example of the Record definition syntax, it's mediocre at
-- best, but it gets the job dome without having to write out a huge number
-- of constructors and wrappers for each element.
exampleRecord :: RecordCons TypeVal
exampleRecord
  = ["Number"  <:= TVInt    $ 3
    ,"Name"    <~= TCString $ oneOf ["a","b","c"]
    ,"record1" <~= TCRecord $
      [ "par1"   <:= TVFloat  $ 2
      , "pas2"   <~= TCInt    $ lessThan 3
      , "pas3"   <~= TCInt    $ [lessThanEq 3, greaterThan 1, oneOf [2,3,4]]
      ]
    ,"record2" <:= TVRecord $
      [ "foo1"   <:= TVInt    $ 1
      , "foo2"   <:= TVString $ "test"
      ]
    ]

--  testPort :: Port
--  testPort = makePort "portName" do
--    mkType [
--        "kind"       <:= TVString $ "Power In"
--      , "voltage"    <~= TCFloat  $ [lessThan 3.35, greaterThan 3.25]
--      , "maxCurrent" <:= TVFloat  $ 1.2
--      ]
--
--    linkType "Power"
--
--    constraint $ "type.voltage" .== "link.voltage"
--
--  testModule :: Module
--  testModule = makeModule "moduleName" do
--    gpios <- duplicate 8 "gpio" $ gpioPort [
--        "voltage" <:= TVFloat $ 3.3
--      , "current" <:= TVFloat $ 0.01
--      ]
--
--    spi <- spiPort ["baudRate" <:= TVFloat $ 3.2]
--
--    mutuallyExclusive [gpios !! 1,gpios !! 3,gpios !! 6] [spi]



-- | Same Deal with kinds, we can tie the knot with Fix, and get something
--   more useful than just this simple witness for the kinds of the first
--   set of types.
--
--   This is especially useful when we're comparing types of elements
--   beforehand and want to split sub-records off into separate bins that
--   are kept merged as needed.
data KindVal' a where
  KVBool   :: KindVal' a
  KVFloat  :: KindVal' a
  KVInt    :: KindVal' a
  KVString :: KindVal' a
  KVUID    :: KindVal' a
  -- Sometimes we want to store records as maps to more records, othertimes
  -- we want to store them as sets of TypeIDs that might be stored in a
  -- database. It depends on what constext we're using them in.
  KVRecord :: a -> KindVal' a
  KVTop    :: KindVal' a
  KVBottom :: KindVal' a
  deriving (Show,Read,Eq)

-- | Sadly Yes, we need this newtype and can't just use Fix.
newtype KindVal = KindVal { unKV :: KindVal' (Map String KindVal)}
  deriving (Show, Read, Eq)

instance Newtype KindVal (KindVal' (Map String KindVal)) where
  pack = KindVal
  unpack = unKV

-- | This is the primordial function that we use to get a kind from a
--   corresponding TypeVal.
--
--   It takes a bit of effort, but you can make f work for you in a
--   huge number of contexts. It can get the type of the sub-record, store
--   that in a DB, and return the new ID. With judicious unpacks packs and a
--   recursive call it can just be the pure tracnsform, etc..
--
--   If you need help, let the type guide you, use typed holes to figure out
--   what need to happen. Everything should be doable with the tools from
--   Functor, Applicative, Monad, Newtype, and the KindVal constructors.
getTVKindM :: Monad m => (Record' a -> m (KindVal' b)) -> TypeVal' a -> m (KindVal' b)
getTVKindM _ (TVBool   _) = return KVBool
getTVKindM _ (TVFloat  _) = return KVFloat
getTVKindM _ (TVInt    _) = return KVInt
getTVKindM _ (TVString _) = return KVString
getTVKindM _ (TVUID    _) = return KVUID
getTVKindM f (TVRecord m) = f m

-- | The pure version of the function to get the kind of a TypeVal.
getTVKind :: TypeVal -> KindVal
getTVKind = runIdentity . getKind
  where
    getKind :: TypeVal -> Identity KindVal
    getKind = fmap pack . getTVKindM (fmap KVRecord . mapM getKind . unpack) . unpack

-- | Similar to getTVKindM but with a few more complications, because the
--   input function works on ambiguous values you need to manually disassemble
--   and reassemble the Ambiguous returning things correctly.
--
--   Again, types are your friend, just do what they ask you in order to get
--   the result you need.
getTCKindM :: Monad m => (Constraints (Record' a) -> m (KindVal' b)) -> TypeCons' a -> m (KindVal' b)
getTCKindM _ (TCBool   _) = return KVBool
getTCKindM _ (TCFloat  _) = return KVFloat
getTCKindM _ (TCInt    _) = return KVInt
getTCKindM _ (TCString _) = return KVString
getTCKindM _ (TCUID    _) = return KVUID
getTCKindM f (TCRecord r) = f r
getTCKindM _ TCBottom     = return KVBottom
getTCKindM _ TCTop        = return KVTop

-- | Pure version of the getTCKind specialized for the standard KindValue,
--   should be good enough, even if it's a bit ugly.
--
--   TODO :: There's a lot of missing checking here that really should use a
--           levitated lattice structure and a bunch of checking to ensure
--           that Top and Bottom get propgated outwards as far as possible.
--           With that we'd get much cleaner code all around, even if there's
--           still a lot of packing and unpacking.
--
getTCKind :: TypeCons -> KindVal
getTCKind = runIdentity . getKind
  where
    getKind :: TypeCons -> Identity KindVal
    getKind = fmap pack . getTCKindM getRCKind . unpack

    getRCKind :: RecordCons TypeVal -> Identity (KindVal' (Map String KindVal))
    getRCKind  RCTop      = return KVTop
    getRCKind  RCBottom   = return KVBottom
    getRCKind (RCAmbig m) = KVRecord <$> mapM getAmbigKind m

    getAmbigKind :: Ambiguous TypeVal -> Identity KindVal
    getAmbigKind  Impossible  = return . pack $ KVTop
    getAmbigKind (Concrete v) = return $ getTVKind v
    getAmbigKind (Abstract c) = getKind c
