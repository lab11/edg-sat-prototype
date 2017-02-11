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

data ValRef = ValRef {
} deriving (Show, Read, Eq)

makeLensesWith abbreviatedFields ''ValRef

data ValSBV = ValSBV {
} deriving (Show, Read, Eq)

makeLensesWith abbreviatedFields ''ValSBV

data RecRef = RecRef {
} deriving (Show, Read, Eq)

makeLensesWith abbreviatedFields ''RecRef

data RecSBV = RecSBV {
} deriving (Show, Read, Eq)

makeLensesWith abbreviatedFields ''RecSBV
