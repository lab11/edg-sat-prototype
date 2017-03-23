
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
import Data.String

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

import Control.Monad.Trans.Class
-- import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit
import Control.Monad.Ether.Implicit.Writer
import Control.Monad.Ether.Implicit.Reader
import Control.Monad.Ether.Implicit.Except
import Control.Monad.Ether.Implicit.State.Strict
import Control.Lens.TH


import GHC.Generics
import Control.DeepSeq

import Control.Lens.TH
import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Library.Types
import EDG.Predicates
import EDG.Expression

-- | Tagged type we'll be using as references that can cross the Gather/SBV
--   boundary.
newtype Ref a = Ref {unRef :: String}
  deriving (Show, Read, Eq, Ord, Generic, NFData)

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
  deriving (Show, Read, Eq, Ord, Generic, NFData)

instance Newtype ValEqClass Integer where
  pack = ValEqClass
  unpack (ValEqClass i) = i

-- | We use these as the names for equality classes for records
newtype RecEqClass = RecEqClass Integer
  deriving (Show, Read, Eq, Ord, Generic, NFData)

instance Newtype RecEqClass Integer where
  pack = RecEqClass
  unpack (RecEqClass i) = i

data ValInfo = ValInfo {
    -- Reference to the integer in which we store kinds for
    -- disambiguation.
    viKindRef :: Ref Integer
    -- Reference to the actual stored value.
  , viValRef  :: ValRef
} deriving (Show, Read, Eq, Generic, NFData)

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
} deriving (Show, Eq, Read, Generic, NFData)


data RecSBV = RecSBV {
  -- the elems are (<is field used in the record?>,<value of field>)
    rsFields :: Map String (SBV Bool,ValueSBV)
  -- possibly a name, only for debugging purposes
  , rsRefName :: Maybe (String)
} deriving (Show, Eq)

-- | Phantom type we'll use to flag things as related to Ports
data Port
-- | Phantom type we'll use to flag things as related to Modules
data Module
data ModPort
-- | Phantom type we'll use to flag things as related to Links
data Link
data LinkPort
-- | Phantom type we'll use to link to the baseEDG monad
data EDG

-- | Gets the inverse type for these flags, just something to help with getting
--   the compiler to typecheck my stupidity.
type family Flip a :: *
type instance Flip Port = Port
type instance Flip Module = Link
type instance Flip Link   = Module
type instance Flip LinkPort = ModPort
type instance Flip ModPort  = LinkPort

type family PFlip a :: *
type instance PFlip LinkPort = ModPort
type instance PFlip ModPort  = LinkPort

-- | Gets the phantom type for the type of port from the identifier.
type family Portify a :: *
type instance Portify Port = Port
type instance Portify Link   = LinkPort
type instance Portify Module = ModPort

-- Wrapper for resources that we use while
newtype Resource a = Resource String
  deriving (Eq,Ord,Show,Read,Generic, NFData)

instance Newtype (Resource a) String where
  pack = Resource
  unpack (Resource s) = s

instance IsString (Resource a) where
  fromString = pack

-- Exists for debugging purposes.
class Monad m => NamedMonad m where
  monadName :: m String

-- NOTE :: These template haskell things have to be at the end of the
--         file so that we don't mess up the code generation and typing
--         processes.
makeLensesWith abbreviatedFields ''ValInfo
makeLensesWith abbreviatedFields ''ValueSBV
makeLensesWith abbreviatedFields ''RecInfo
makeLensesWith abbreviatedFields ''RecSBV
