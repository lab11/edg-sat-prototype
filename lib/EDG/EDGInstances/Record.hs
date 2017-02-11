
module EDG.EDGInstances.Record where

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
import EDG.EDGMonad
import EDG.EDGInstances.Bool
import EDG.SBVWrap
import EDG.EDGDatatype

instance SBVAble Value where

  type SBVType Value = ValSBV

  type RefType Value = ValRef

  ref :: String -> EDGMonad (ValRef)
  ref = undefined

  refConcrete :: String -> Value -> EDGMonad ValRef
  refConcrete = undefined

  refAbstract :: String -> Constrained -> EDGMonad ValRef
  refAbstract = undefined

  sbv :: ValRef -> SBVMonad ValSBV
  sbv = undefined

  lit :: Value -> SBVMonad ValSBV
  lit = undefined

  add :: ValRef -> ValSBV -> SBVMonad ()
  add = undefined

  getName :: ValRef -> String
  getName = undefined

  fixConcrete :: Value -> EDGMonad Value
  fixConcrete = undefined

  fixAbstract :: Constrained -> EDGMonad Constrained
  fixAbstract = undefined

class InvertSBV Value where

  extract :: Modelable a => DecodeState -> a -> ValRef -> Maybe Value
  extract = undefined

class EDGEquals Value where

  equalE :: ValRef -> ValRef -> String -> EDGMonad (Ref Bool)
  equalE = undefined

  unequalE :: ValRef -> ValRef -> String -> EDGMonad (Ref Bool)
  unequalE = undefined

instance SBVAble Record where

  type SBVType Record = RecSBV

  type RefType Record = RecRef

  ref :: String -> EDGMonad (RrcRef)
  ref = undefined

  refConcrete :: String -> Record -> EDGMonad ValRef
  refConcrete = undefined

  refAbstract :: String -> RecCons -> EDGMonad ValRef
  refAbstract = undefined

  sbv :: RecRef -> SBVMonad RecSBV
  sbv = undefined

  lit :: Record -> SBVMonad RecordSBV
  lit = undefined

  add :: RecRef -> RrcSBV -> SBVMonad ()
  add = undefined

  getName :: RecRef -> String
  getName = undefined

  fixConcrete :: Record -> EDGMonad Record
  fixConcrete = undefined

  fixAbstract :: RecCons -> EDGMonad RecCons
  fixAbstract = undefined

class InvertSBV Record where

  extract :: Modelable a => DecodeState -> a -> RecRef -> Maybe Record
  extract = undefined

class EDGEquals Value where

  equalE :: RecRef -> RecRef -> String -> EDGMonad (Ref Bool)
  equalE = undefined

  unequalE :: RecRef -> RecRef -> String -> EDGMonad (Ref Bool)
  unequalE = undefined
