
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

import Data.Void

-- Value that captures the various types of things with an assiciated kind.
data Kinded a b where
  Int    :: (KVAble a) => KInt    a   -> Kinded a b
  Bool   :: (KVAble a) => KBool   a   -> Kinded a b
  Float  :: (KVAble a) => KFloat  a   -> Kinded a b
  String :: (KVAble a) => KString a   -> Kinded a b
  UID    :: (KVAble a) => KUID    a   -> Kinded a b
  Record :: (KVAble a) => KRecord a b -> Kinded a b
  KVTop  :: (KVAble a) => KTop    a   -> Kinded a b
  KVBot  :: (KVAble a) => KBottom a   -> Kinded a b

deriving instance (Eq (KInt a), Eq (KBool a), Eq (KFloat a), Eq (KString a)
  , Eq (KUID a), Eq (KRecord a b), Eq (KTop a), Eq (KBottom a)
  ) => Eq (Kinded a b)

deriving instance (Show (KInt a), Show (KBool a), Show (KFloat a)
  , Show (KString a), Show (KUID a), Show (KRecord a b), Show (KTop a)
  , Show (KBottom a)) => Show (Kinded a b)

deriving instance (KVAble a, Read (KInt a), Read (KBool a), Read (KFloat a)
  , Read (KString a), Read (KUID a), Read (KRecord a b), Read (KTop a)
  , Read (KBottom a)) => Read (Kinded a b)

-- | Gets a separate integer for each constructor
getKindNum :: Kinded a b -> Integer
getKindNum KVTop{}  = 0
getKindNum KVBot{}  = 1
getKindNum Int{}    = 2
getKindNum Bool{}   = 3
getKindNum Float{}  = 4
getKindNum String{} = 5
getKindNum UID{}    = 6
getKindNum Record{} = 7

-- | The typeclass that lets us
class KVAble a where
  type KInt    a :: *
  type KBool   a :: *
  type KFloat  a :: *
  type KString a :: *
  type KUID    a :: *
  type KRecord a b :: *
  type KTop    a :: *
  type KBottom a :: *

-- | This flag lets us define concrete instances of a kinded value, that
--   hold specific instances of elements as needed.
data Val

instance KVAble Val where
  type KInt    Val   = Integer
  type KBool   Val   = Bool
  type KFloat  Val   = Float
  type KString Val   = String
  type KUID    Val   = UID'
  type KRecord Val b = Record' b
  type KTop    Val   = Void
  type KBottom Val   = Void

-- | This is the basic Value, which disambiguates between each of the types
--   we have handy, each constructor can also act as a value level witness if
--   we need one.
type Value' = Kinded Val

-- | This is the fixed point we're going to be working with when we have a
--   typed value lying aorund.
newtype Value = Value { getVal :: Value' Value}
  deriving (Eq, Show, Read)

instance Newtype Value (Value' Value) where
  pack   = Value
  unpack = getVal

-- | This time we fill the type with unit so that we can use the flags
--   themselves as markers for the kind of a value.
data Knd

instance KVAble Knd where
  type KInt    Knd   = ()
  type KBool   Knd   = ()
  type KFloat  Knd   = ()
  type KString Knd   = ()
  type KUID    Knd   = ()
  type KRecord Knd b = Map String b
  type KTop    Knd   = ()
  type KBottom Knd   = ()

type Kind' = Kinded Knd

newtype Kind = Kind { getKnd :: Kind' Kind }
  deriving (Show, Read, Eq)

instance Newtype Kind (Kind' Kind) where
  pack = Kind
  unpack = getKnd

-- | This one is for defining constraints over Values, it pretty much just
--   uses the corresponding constraints type whenever possible.
data Cons

instance KVAble Cons where
  type KInt    Cons   = Constraints Integer
  type KBool   Cons   = Constraints Bool
  type KFloat  Cons   = Constraints Float
  type KString Cons   = Constraints String
  type KUID    Cons   = Constraints UID'
  type KRecord Cons b = Constraints (Record' b)
  type KTop    Cons   = ()
  type KBottom Cons   = ()

-- | And this is the corresponding type for the constraints over a TValue.
--   Same deal with the parameterization.
--
--   I'm getting the constraints here rather that just typing it out because
--   it prevents user error if we change the type of some constraint.
--
--   TODO :: Feh, I'm seriously hating having to put all these explicit bottoms
--           and tops in all of our constraint dataTypes, they
type Constrained' = Kinded Cons

-- | Keep in mind that `Value' a` corresponds with `Constrained' a` not
--   `Constrained' (Constraints a)`. Since we use the typeclass to pick the
--   relevant constraint.
--
--   However we still need a newtype because otherwise our choice for predicate
--   domain would leave us with a non-injective type family for the predicate
--   domains.
newtype Constrained = Constrained { getCons :: Constrained' Value}
  deriving (Show, Read, Eq)

instance Newtype Constrained (Constrained' Value) where
  pack = Constrained
  unpack = getCons

-- | Convinience alias for the type of record that most people will actually
--   be working with (Yes, there's a lot of extra `Fix` es whenever you try
--   to pattern match.)
type Record = Record' Value

type RecCons = RecordCons Value

instance AsPredicate (Constrained' a) where
  type PredicateDomain (Constrained' a) = (Value' a)
  asPredicate (Int    c) (Int    v) = asPredicate c v
  asPredicate (Bool   c) (Bool   v) = asPredicate c v
  asPredicate (Float  c) (Float  v) = asPredicate c v
  asPredicate (String c) (String v) = asPredicate c v
  asPredicate (UID    c) (UID    v) = asPredicate c v
  asPredicate (Record c) (Record v) = asPredicate c v
  asPredicate (KVBot  _)  _         = True
  asPredicate  _          _         = False

instance AsPredicate Constrained where
  type PredicateDomain Constrained = Value
  asPredicate (unpack -> c) (unpack -> v) = asPredicate c v

instance SATAblePredicate (Constrained' a) where
  isSAT (Int    c) = isSAT c
  isSAT (Bool   c) = isSAT c
  isSAT (Float  c) = isSAT c
  isSAT (String c) = isSAT c
  isSAT (UID    c) = isSAT c
  isSAT (Record c) = isSAT c
  isSAT (KVBot  _) = True
  isSAT (KVTop  _) = False

instance SATAblePredicate Constrained where
  isSAT = isSAT . unpack

instance BottomPredicate (Constrained' a) where
  isBottom (KVBot _) = True
  -- | The idea here is that unless you've actually specified bottom, you've
  --   still constrained the kind of the value. Even if the constraint is
  --   bottom *for that Kind* it'll reject inputs of other kinds.
  isBottom  _        = False

instance BottomPredicate Constrained where
  isBottom = isBottom . unpack

instance CollapseablePredicate (Constrained' a) where
  collapse (Int    c) = Int    <$> collapse c
  collapse (Bool   c) = Bool   <$> collapse c
  collapse (Float  c) = Float  <$> collapse c
  collapse (String c) = String <$> collapse c
  collapse (UID    c) = UID    <$> collapse c
  collapse (Record c) = Record <$> collapse c
  collapse (KVBot  _) = Nothing
  collapse (KVTop  _) = Nothing

instance CollapseablePredicate Constrained where
  collapse = fmap pack . collapse . unpack

instance (Eq a,Constrainable a) => LiftablePredicate (Constrained' a) where
  liftPredicate (Int    v) = Int    $ liftPredicate v
  liftPredicate (Bool   v) = Bool   $ liftPredicate v
  liftPredicate (Float  v) = Float  $ liftPredicate v
  liftPredicate (String v) = String $ liftPredicate v
  liftPredicate (UID    v) = UID    $ liftPredicate v
  liftPredicate (Record v) = Record $ liftPredicate v
  liftPredicate (KVBot  v) = absurd v
  liftPredicate (KVTop  v) = absurd v

instance LiftablePredicate Constrained where
  liftPredicate = pack . liftPredicate . unpack

instance (Eq a, Constrainable a) => Constrainable (Value' a) where
  type Constraints (Value' a) = Constrained' a

instance Constrainable Value where
  type Constraints Value = Constrained

instance (Eq a,Constrainable a, PartialOrd (Constraints a)) => PartialOrd (Constrained' a) where
  leq KVBot{} _ = True
  leq _ KVBot{} = False
  leq _ a | isSAT a = True
  leq a _ | isSAT a = False
  leq (Int    c) (Int    c') = c `leq` c'
  leq (Bool   c) (Bool   c') = c `leq` c'
  leq (Float  c) (Float  c') = c `leq` c'
  leq (String c) (String c') = c `leq` c'
  leq (UID    c) (UID    c') = c `leq` c'
  leq (Record c) (Record c') = c `leq` c'
  leq _ _ = False

instance PartialOrd Constrained where
  leq (unpack -> c) (unpack -> c') = c `leq` c'

instance (Eq a,Constrainable a, JoinSemiLattice (Ambiguous a)) => JoinSemiLattice (Constrained' a) where
  (\/) a _ | isSAT a = KVTop ()
  (\/) _ a | isSAT a = KVTop ()
  (\/) KVBot{} a = a
  (\/) a KVBot{} = a
  (\/) (Int    c) (Int    c') = Int    $ c \/ c'
  (\/) (Bool   c) (Bool   c') = Bool   $ c \/ c'
  (\/) (Float  c) (Float  c') = Float  $ c \/ c'
  (\/) (String c) (String c') = String $ c \/ c'
  (\/) (UID    c) (UID    c') = UID    $ c \/ c'
  (\/) (Record c) (Record c') = Record $ c \/ c'
  (\/) _ _ = KVTop ()

instance JoinSemiLattice Constrained where
  (\/) (unpack -> c) (unpack -> c') = pack $ c \/ c'

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (Eq a,Constrainable a, MeetSemiLattice (Ambiguous a)) => MeetSemiLattice (Constrained' a) where
  (/\) KVBot{} _ = KVBot ()
  (/\) _ KVBot{} = KVBot ()
  (/\) a b | isSAT a = b
  (/\) b a | isSAT a = b
  (/\) (Int    c) (Int    c') = Int    $ c /\ c'
  (/\) (Bool   c) (Bool   c') = Bool   $ c /\ c'
  (/\) (Float  c) (Float  c') = Float  $ c /\ c'
  (/\) (String c) (String c') = String $ c /\ c'
  (/\) (UID    c) (UID    c') = UID    $ c /\ c'
  (/\) (Record c) (Record c') = Record $ c /\ c'
  (/\) _ _ = KVBot ()


-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (MeetSemiLattice (Ambiguous Value)) => MeetSemiLattice Constrained where
  (/\) (unpack -> c) (unpack -> c') = pack $ c /\ c'

instance (Eq a,Constrainable a,JoinSemiLattice (Ambiguous a)) => BoundedJoinSemiLattice (Constrained' a) where
  bottom = KVBot ()

instance BoundedJoinSemiLattice Constrained where
  bottom = pack bottom

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (Eq a,Constrainable a,MeetSemiLattice (Ambiguous a)) => BoundedMeetSemiLattice (Constrained' a) where
  top = KVTop ()

-- TODO :: Remove constraint when you write meet instances for Ambiguous
instance (MeetSemiLattice (Ambiguous Value)) => BoundedMeetSemiLattice Constrained where
  top = pack top

-- | This is where we start doing more interesting things with our RecordCons
--   and all the nested types we've got lying around. This lets us present
--   a much nicer interface to the user while preserving all the power in the
--   backend.
--
--   TODO :: Make sure there's no sensible list interface for these things, at
--           least insofar as pattern matching is concerned.
--
instance IsList (RecordCons Value) where
  type Item (RecordCons Value) = (String,Ambiguous Value)

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
  type Item Record = (String,Ambiguous Value)
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
(<:=) :: String -> (t -> Value' Value) -> t -> (String,Ambiguous Value)
(<:=) s c v = (s,Concrete . pack . c $ v)

-- | This operator allows you define an ambiguous field, with a series of
--   constraints on it. Each is defined as:
--
--      ` <Name of Field> <~= <TC Constructor for field type> $ <Constraint over field>`
--   or `(<Name of Field> <~= <TC Constructor for field type>)  <Constraint over field>`
(<~=) :: String -> (Constraints t -> Constrained' Value) -> Constraints t -> (String,Ambiguous Value)
(<~=) s c v = (s,flattenAmbig . Abstract . pack . c $ v)

-- This is just an example of the Record definition syntax, it's mediocre at
-- best, but it gets the job dome without having to write out a huge number
-- of constructors and wrappers for each element.
exampleRecord :: RecordCons Value
exampleRecord
  = ["Number"  <:= Int    $ 3
    ,"Name"    <~= String $ oneOf ["a","b","c"]
    ,"record1" <~= Record $
      [ "par1"   <:= Float  $ 2
      , "pas2"   <~= Int    $ lessThan 3
      , "pas3"   <~= Int    $ [lessThanEq 3, greaterThan 1, oneOf [2,3,4]]
      ]
    ,"record2" <:= Record $
      [ "foo1"   <:= Int    $ 1
      , "foo2"   <:= String $ "test"
      ]
    ]
--
--  --  testPort :: Port
--  --  testPort = makePort "portName" do
--  --    mkType [
--  --        "kind"       <:= TVString $ "Power In"
--  --      , "voltage"    <~= TCFloat  $ [lessThan 3.35, greaterThan 3.25]
--  --      , "maxCurrent" <:= TVFloat  $ 1.2
--  --      ]
--  --
--  --    linkType "Power"
--  --
--  --    constraint $ "type.voltage" .== "link.voltage"
--  --
--  --  testModule :: Module
--  --  testModule = makeModule "moduleName" do
--  --    gpios <- duplicate 8 "gpio" $ gpioPort [
--  --        "voltage" <:= TVFloat $ 3.3
--  --      , "current" <:= TVFloat $ 0.01
--  --      ]
--  --
--  --    spi <- spiPort ["baudRate" <:= TVFloat $ 3.2]
--  --
--  --    mutuallyExclusive [gpios !! 1,gpios !! 3,gpios !! 6] [spi]
--
--
--
--  -- | Same Deal with kinds, we can tie the knot with Fix, and get something
--  --   more useful than just this simple witness for the kinds of the first
--  --   set of types.
--  --
--  --   This is especially useful when we're comparing types of elements
--  --   beforehand and want to split sub-records off into separate bins that
--  --   are kept merged as needed.
--  data KindVal' a where
--    KVBool   :: KindVal' a
--    KVFloat  :: KindVal' a
--    KVInt    :: KindVal' a
--    KVString :: KindVal' a
--    KVUID    :: KindVal' a
--    -- Sometimes we want to store records as maps to more records, othertimes
--    -- we want to store them as sets of TypeIDs that might be stored in a
--    -- database. It depends on what constext we're using them in.
--    KVRecord :: a -> KindVal' a
--    KVTop    :: KindVal' a
--    KVBottom :: KindVal' a
--    deriving (Show,Read,Eq)
--
--  -- | Sadly Yes, we need this newtype and can't just use Fix.
--  newtype KindVal = KindVal { unKV :: KindVal' (Map String KindVal)}
--    deriving (Show, Read, Eq)
--
--  instance Newtype KindVal (KindVal' (Map String KindVal)) where
--    pack = KindVal
--    unpack = unKV
--
--  -- | This is the primordial function that we use to get a kind from a
--  --   corresponding TypeVal.
--  --
--  --   It takes a bit of effort, but you can make f work for you in a
--  --   huge number of contexts. It can get the type of the sub-record, store
--  --   that in a DB, and return the new ID. With judicious unpacks packs and a
--  --   recursive call it can just be the pure tracnsform, etc..
--  --
--  --   If you need help, let the type guide you, use typed holes to figure out
--  --   what need to happen. Everything should be doable with the tools from
--  --   Functor, Applicative, Monad, Newtype, and the KindVal constructors.
--  getTVKindM :: Monad m => (Record' a -> m (KindVal' b)) -> TypeVal' a -> m (KindVal' b)
--  getTVKindM _ (TVBool   _) = return KVBool
--  getTVKindM _ (TVFloat  _) = return KVFloat
--  getTVKindM _ (TVInt    _) = return KVInt
--  getTVKindM _ (TVString _) = return KVString
--  getTVKindM _ (TVUID    _) = return KVUID
--  getTVKindM f (TVRecord m) = f m
--
--  -- | The pure version of the function to get the kind of a TypeVal.
--  getTVKind :: TypeVal -> KindVal
--  getTVKind = runIdentity . getKind
--    where
--      getKind :: TypeVal -> Identity KindVal
--      getKind = fmap pack . getTVKindM (fmap KVRecord . mapM getKind . unpack) . unpack
--
--  -- | Similar to getTVKindM but with a few more complications, because the
--  --   input function works on ambiguous values you need to manually disassemble
--  --   and reassemble the Ambiguous returning things correctly.
--  --
--  --   Again, types are your friend, just do what they ask you in order to get
--  --   the result you need.
--  getTCKindM :: Monad m => (Constraints (Record' a) -> m (KindVal' b)) -> TypeCons' a -> m (KindVal' b)
--  getTCKindM _ (TCBool   _) = return KVBool
--  getTCKindM _ (TCFloat  _) = return KVFloat
--  getTCKindM _ (TCInt    _) = return KVInt
--  getTCKindM _ (TCString _) = return KVString
--  getTCKindM _ (TCUID    _) = return KVUID
--  getTCKindM f (TCRecord r) = f r
--  getTCKindM _ TCBottom     = return KVBottom
--  getTCKindM _ TCTop        = return KVTop
--
--  -- | Pure version of the getTCKind specialized for the standard KindValue,
--  --   should be good enough, even if it's a bit ugly.
--  --
--  --   TODO :: There's a lot of missing checking here that really should use a
--  --           levitated lattice structure and a bunch of checking to ensure
--  --           that Top and Bottom get propgated outwards as far as possible.
--  --           With that we'd get much cleaner code all around, even if there's
--  --           still a lot of packing and unpacking.
--  --
--  getTCKind :: TypeCons -> KindVal
--  getTCKind = runIdentity . getKind
--    where
--      getKind :: TypeCons -> Identity KindVal
--      getKind = fmap pack . getTCKindM getRCKind . unpack
--
--      getRCKind :: RecordCons TypeVal -> Identity (KindVal' (Map String KindVal))
--      getRCKind  RCTop      = return KVTop
--      getRCKind  RCBottom   = return KVBottom
--      getRCKind (RCAmbig m) = KVRecord <$> mapM getAmbigKind m
--
--      getAmbigKind :: Ambiguous TypeVal -> Identity KindVal
--      getAmbigKind  Impossible  = return . pack $ KVTop
--      getAmbigKind (Concrete v) = return $ getTVKind v
--      getAmbigKind (Abstract c) = getKind c
