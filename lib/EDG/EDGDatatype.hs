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
  type KInt    VRef   = Ref Integer
  type KBool   VRef   = Ref Bool
  type KFloat  VRef   = Ref Float
  type KString VRef   = Ref String
  type KUID    VRef   = Ref UID'
  type KRecord VRef b = Ref Record
  type KTop    VRef   = Void
  type KBottom VRef   = ()

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

-- | We use these as names for equality classes for kinds.
type EqClassID = Integer

-- | Information we have about each equality class.
data ValInfo = ValInfo {
    viEqClass  :: EqClassID
  , viValRef   :: ValRef
  , viKindRef  :: Ref Integer
} deriving (Show, Read, Eq)


data ValueSBV = ValueSBV {
    vsKindSBV :: SBV Integer
  , vsValSBV  :: ValSBV
} deriving (Show, Eq)


data RecInfo = RecInfo {
    -- | known and assigned fields of the record.
    riFields  :: Map String (Ref Value)
  , riEqClass :: EqClassID
} deriving (Show, Eq, Read)


data RecSBV = RecSBV {
    rsFields :: Map String ValueSBV
} deriving (Show, Eq)

-- NOTE :: These template haskell things have to be at the end of the
--         file so that we don't mess up the code generation and typing
--         processes.
makeLensesWith abbreviatedFields ''ValInfo
makeLensesWith abbreviatedFields ''ValueSBV
makeLensesWith abbreviatedFields ''RecInfo
makeLensesWith abbreviatedFields ''RecSBV
