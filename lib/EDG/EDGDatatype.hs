{-# LANGUAGE UndecidableInstances #-}

module EDG.EDGDatatype where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Control.Newtype

import Data.Void

import Control.Monad.Ether.Implicit
import Control.Monad.MonadSymbolic
import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
  )
import Data.SBV.Internals (
  CWVal(..)
  )
import qualified Data.SBV as S
import Control.Monad.Scribe
import Control.Monad.Identity (Identity)

import Control.Lens.Ether.Implicit
import Control.Lens.TH

import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Library.Types
import EDG.Predicates

-- | Tagged type we'll be using as references that can cross the Gather/SBV
--   boundary.
newtype Ref a = Ref {unRef :: String}
  deriving (Show, Read, Eq, Ord)

instance Newtype (Ref a) String where
  pack = Ref
  unpack = unRef

-- | This time we fill the type with unit so that we can use the flags
--   themselves as markers for the kind of a value.
data VRef

instance KVAble VRef where
  -- If you have an actual value then just store the reference to it
  -- once the type is at least this determined, then you'll never
  -- be able to safely constrain it more.
  type KInt    VRef   = Ref Integer
  type KBool   VRef   = Ref Bool
  type KFloat  VRef   = Ref Float
  type KString VRef   = Ref String
  type KUID    VRef   = Ref UID'
  type KRecord VRef b = Ref Record
  -- If you don't have a concrete type yet, then you're in a class
  -- of elements that share the same (as yet unknown) type.
  type KBottom VRef   = ValEqClass
  -- Unused Constructors
  type KTop    VRef   = Void

type ValRef = Kinded VRef ()

data VSBV

instance KVAble VSBV where
  type KInt    VSBV   = SBV Integer
  type KBool   VSBV   = SBV Bool
  type KFloat  VSBV   = SBV Float
  type KString VSBV   = SBV String
  type KUID    VSBV   = SBV UID'
  type KRecord VSBV b = RecSBV
  type KTop    VSBV   = Void
  type KBottom VSBV   = ()

type ValSBV = Kinded VSBV ()

-- | We use these as names for equality classes for values that
--   have an unfixed kind.
newtype ValEqClass = ValEqClass Integer
  deriving (Show, Read, Eq, Ord)

instance Newtype ValEqClass Integer where
  pack = ValEqClass
  unpack (ValEqClass i) = i

-- | We use these as the names for equality classes for records
newtype RecEqClass = RecEqClass Integer
  deriving (Show, Read, Eq, Ord)

instance Newtype RecEqClass Integer where
  pack = RecEqClass
  unpack (RecEqClass i) = i

data ValInfo = ValInfo {
    -- Reference to the integer in which we store kinds for
    -- disambiguation.
    viKindRef :: Ref Integer
    -- Reference to the actual stored value.
  , viValRef  :: ValRef
} deriving (Show, Read, Eq)

data ValueSBV = ValueSBV {
  -- The stored integer
    vsKindSBV :: SBV Integer
  -- The stored value
  , vsValSBV  :: ValSBV
  -- Possibly a name (only for debugging purposes)
  , vsRefName :: Maybe (String)
} deriving (Show, Eq)


data RecInfo = RecInfo {
    -- | known and assigned fields of the record.
    riFields  :: Map String (Ref Bool, Ref Value)
  , riEqClass :: RecEqClass
} deriving (Show, Eq, Read)


data RecSBV = RecSBV {
  -- the elems are (<is field used in the record?>,<value of field>)
    rsFields :: Map String (SBV Bool,ValueSBV)
  -- possibly a name, only for debugging purposes
  , rsRefName :: Maybe (String)
} deriving (Show, Eq)

-- Wrapper for resources that we use while
newtype Resource = Resource String

-- | Type for arbitrary expressions within the system.
--
--   First param is a flag that detmines how we describe values
--   or references
--
--   Second is a phantom type we use to get compile time errors.
data Exp a t where
  -- | Static values
  Val :: Expressible a => ExpValue a Value -> Exp a t
  -- | Pointers to values that are reasonable in some
  --   context.
  Pnt :: Expressible a => ExpPointer a Value -> Exp a t
  -- | Equality Ops
  (:==)   :: Exp a (ExpEq t) -> Exp a (ExpEq t) -> Exp a Bool
  (:/=)   :: Exp a (ExpEq t) -> Exp a (ExpEq t) -> Exp a Bool
  -- | Boolean comparison ops
  (:&&)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
  (:||)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
  (:~&)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
  (:~|)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
  (:<+>)  :: Exp a Bool -> Exp a Bool -> Exp a Bool
  (:=>)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
  Not     :: Exp a Bool -> Exp a Bool
  JustOne :: [Exp a Bool] -> Exp a Bool
  All     :: [Exp a Bool] -> Exp a Bool
  Any     :: [Exp a Bool] -> Exp a Bool
  -- | Ordering Ops
  (:<)  :: Exp a (ExpOrd t) -> Exp a (ExpOrd t) -> Exp a Bool
  (:<=) :: Exp a (ExpOrd t) -> Exp a (ExpOrd t) -> Exp a Bool
  (:>)  :: Exp a (ExpOrd t) -> Exp a (ExpOrd t) -> Exp a Bool
  (:>=) :: Exp a (ExpOrd t) -> Exp a (ExpOrd t) -> Exp a Bool
  -- | Numerical Ops
  Negate :: Exp a (ExpNum t) -> Exp a (ExpNum t)
  (:+)   :: Exp a (ExpNum t) -> Exp a (ExpNum t) -> Exp a (ExpNum t)
  (:-)   :: Exp a (ExpNum t) -> Exp a (ExpNum t) -> Exp a (ExpNum t)
  (:*)   :: Exp a (ExpNum t) -> Exp a (ExpNum t) -> Exp a (ExpNum t)
  (:/)   :: Exp a (ExpNum t) -> Exp a (ExpNum t) -> Exp a (ExpNum t)
  Sum    :: [Exp a (ExpNum t)] -> Exp a (ExpNum t)
  Mult   :: [Exp a (ExpNum t)] -> Exp a (ExpNum t)
  -- | Control Ops
  If :: Exp a Bool -> Exp a (ExpEq t) -> Exp a (ExpEq t) -> Exp a (ExpEq t)
  -- | Other Utility Ops
  -- Returns the number of bool values that are true
  Count :: [Exp a Bool] -> Exp a Integer

type family ExpNum t = t' | t' -> t
type instance ExpNum Integer = Integer
type instance ExpNum Float = Float

type family ExpOrd t = t' | t' -> t
type instance ExpOrd Integer = Integer
type instance ExpOrd Float = Float

type family ExpEq t = t' | t' -> t
type instance ExpEq Value = Value
type instance ExpEq Integer = Integer
type instance ExpEq Float  = Float
type instance ExpEq Bool   = Bool
type instance ExpEq UID'   = UID'
type instance ExpEq Record = Record
-- class ExpNum t
--
-- instance ExpNum Integer
-- instance ExpNum Float

-- | Basic instances for the elements
--
--   NOTE :: This is why Undecidable instances is needed
deriving instance (
    Expressible a
  , Eq (ExpValue a t), Eq (ExpPointer a t)
  , Eq (ExpValue a (ExpEq t)), Eq (ExpPointer a (ExpEq t))
  , Eq (ExpValue a (ExpOrd t)), Eq (ExpPointer a (ExpOrd t))
  , Eq (ExpValue a (ExpNum t)), Eq (ExpPointer a (ExpNum t))
  , Eq (ExpValue a (ExpEq Value)), Eq (ExpPointer a (ExpEq Value))
  , Eq (ExpValue a (ExpOrd Value)), Eq (ExpPointer a (ExpOrd Value))
  , Eq (ExpValue a (ExpNum Value)), Eq (ExpPointer a (ExpNum Value))
  , Eq (ExpValue a Value), Eq (ExpPointer a Value)
  , Eq (ExpValue a (ExpEq Bool)), Eq (ExpPointer a (ExpEq Bool))
  , Eq (ExpValue a (ExpOrd Bool)), Eq (ExpPointer a (ExpOrd Bool))
  , Eq (ExpValue a (ExpNum Bool)), Eq (ExpPointer a (ExpNum Bool))
  , Eq (ExpValue a Bool), Eq (ExpPointer a Bool)
  , Eq (ExpValue a (ExpEq Integer)), Eq (ExpPointer a (ExpEq Integer))
  , Eq (ExpValue a (ExpOrd Integer)), Eq (ExpPointer a (ExpOrd Integer))
  , Eq (ExpValue a (ExpNum Integer)), Eq (ExpPointer a (ExpNum Integer))
  , Eq (ExpValue a Integer), Eq (ExpPointer a Integer)
  ) => Eq (Exp a t)
-- deriving instance (
--     Expressible a
--   , Ord (ExpValue a t), Ord (ExpPointer a t)
--   , Ord (ExpValue a Bool), Ord (ExpPointer a Bool)
--   , Ord (ExpValue a Integer), Ord (ExpPointer a Integer)
--   ) => Ord  (Exp a t)
-- deriving instance (
--     Expressible a
--   , Show (ExpValue a t), Show (ExpPointer a t)
--   , Show (ExpValue a Bool), Show (ExpPointer a Bool)
--   , Show (ExpValue a Integer), Show (ExpPointer a Integer)
--   ) => Show (Exp a t)
-- deriving instance (
--     Expressible a
--   , Read (ExpValue a t), Read (ExpPointer a t)
--   , Read (ExpValue a Bool), Read (ExpPointer a Bool)
--   , Read (ExpValue a Integer), Read (ExpPointer a Integer)
--   ) => Read (Exp a t)

class Expressible a where
  -- The type of the value, and the flag it gets
  type ExpValue   a t
  -- The type of a pointer and the flag it gets
  type ExpPointer a t

-- class Monad m => ExpConvert a m where
--     -- | Static values
--   Val :: Expressible a => ExpValue a t -> Exp a t
--   -- | Pointers to values that are reasonable in some
--   --   context.
--   Pnt :: Expressible a => ExpPointer a t -> Exp a t
--   -- | Equality Ops
--   (:==)   :: Exp a t -> Exp a t -> Exp a Bool
--   (:/=)   :: Exp a t -> Exp a t -> Exp a Bool
--   -- | Boolean comparison ops
--   (:&&)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   (:||)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   (:~&)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   (:~|)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   (:<+>)  :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   (:=>)   :: Exp a Bool -> Exp a Bool -> Exp a Bool
--   Not     :: Exp a Bool -> Exp a Bool
--   JustOne :: [Exp a Bool] -> Exp a Bool
--   All     :: [Exp a Bool] -> Exp a Bool
--   Any     :: [Exp a Bool] -> Exp a Bool
--   -- | Ordering Ops
--   (:<)  :: Exp a t -> Exp a t -> Exp a Bool
--   (:<=) :: Exp a t -> Exp a t -> Exp a Bool
--   (:>)  :: Exp a t -> Exp a t -> Exp a Bool
--   (:>=) :: Exp a t -> Exp a t -> Exp a Bool
--   -- | Numerical Ops
--   (:+)   :: Exp a t -> Exp a t -> Exp a t
--   (:-)   :: Exp a t -> Exp a t -> Exp a t
--   (:*)   :: Exp a t -> Exp a t -> Exp a t
--   (:/)   :: Exp a t -> Exp a t -> Exp a t
--   Sum    :: [Exp a t] -> Exp a t
--   Mult   :: [Exp a t] -> Exp a t
--   Negate :: Exp a t -> Exp a t
--   -- | Control Ops
--   If :: Exp a Bool -> Exp a t -> Exp a t -> Exp a t
--   -- | Other Utility Ops
--   -- Returns the number of bool values that are true
--   Count :: [Exp a Bool] -> Exp a Integer


-- | Cast a particular value as a bool
bool :: Exp a Value -> Exp a Bool
bool = UnsafeCast

-- | Phantom type we'll use to flag things as related to Ports
data PortF

instance Expressible PortF where
  type ExpPointer PortF Value = Ref Value
  type ExpPointer PortF Integer = Ref Integer

-- | Phantom type we'll use to flag things as related to Modules
data ModF
-- | Phantom type we'll use to flag things as related to Links
data LinkF

-- | V
data PortC a = PortC {
    pcID :: Maybe String
  } deriving (Show, Read, Eq, Ord)
-- | Datatype for a description of a port, what is used as input to
--   the problem description
data PortD a = PortD {
    pdID :: String
  } deriving (Show, Read, Eq, Ord)


-- |
-- | Datatype for a description of a port, what is used as input to
--   the system
--
--
-- NOTE :: These template haskell things have to be at the end of the
--         file so that we don't mess up the code generation and typing
--         processes.
makeLensesWith abbreviatedFields ''ValInfo
makeLensesWith abbreviatedFields ''ValueSBV
makeLensesWith abbreviatedFields ''RecInfo
makeLensesWith abbreviatedFields ''RecSBV
