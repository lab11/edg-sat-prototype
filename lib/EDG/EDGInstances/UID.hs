
module EDG.EDGInstances.UID where

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


instance SBVAble UID where

  type SBVType UID = SBV UID

  type RefType UID = Ref UID

  ref :: String -> EDGMonad (Ref UID)
  ref name = let n = Ref name in returnAnd n (sUID name >>= add n)

  refConcrete :: String -> UID -> EDGMonad (Ref UID)
  refConcrete name' s = do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      lv <- lit s
      constrain $ nv S..== lv

  refAbstract :: String -> UIDCons -> EDGMonad (Ref UID)
  refAbstract name' c
    | unSAT c       = throw $ "No valid solution for UID " ++ show name' ++ "."
    | UCTop    <- c = throw $ "No valid solution for UID " ++ show name' ++ "."
    | UCBottom <- c = ref name'
    | UCNew    <- c = do
      n   <- ref name'
      uid <- UID <$> newUID
      returnAnd n $ do
        nv <- sbv n
        lv <- lit uid
        constrain $ nv S..== lv
    | UCVal u <- c = do
      n   <- ref name'
      returnAnd n $ do
        nv <- sbv n
        lv <- lit u
        constrain $ nv S..== lv

  sbv :: Ref UID -> SBVMonad (SBV UID)
  sbv r = do
    val <- uses @SBVS uidRef (Map.lookup r)
    case val of
      Nothing -> throw $ "No ref to UID `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v

  lit :: UID     -> SBVMonad (SBV UID)
  lit = return . reWrap . S.literal . unpack

  add :: Ref UID -> SBV UID -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS uidRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to UID `" ++ show r ++ "` already exists."
      False -> uidRef @SBVS %= (Map.insert r s)

  getName :: Ref UID -> String
  getName = unpack

instance InvertSBV UID where

  extract :: Modelable a => DecodeState -> a -> Ref UID -> Maybe UID
  extract _ model (Ref name) = pack <$> getModelValue name model

instance EDGEquals UID where

  equalE :: Ref UID -> Ref UID -> String -> EDGMonad (Ref Bool)
  equalE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..== bv)

  unequalE :: Ref UID -> Ref UID -> String -> EDGMonad (Ref Bool)
  unequalE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S../= bv)

