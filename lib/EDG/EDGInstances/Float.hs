
module EDG.EDGInstances.Float where

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



instance SBVAble Float where

  type SBVType Float = SBV Float

  type RefType Float = Ref Float

  ref :: String -> EDGMonad (Ref Float)
  ref name = let n = Ref name in returnAnd n (sFloat name >>= add n)

  refConcrete :: String -> Float -> EDGMonad (Ref Float)
  refConcrete name' s = do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      lv <- lit s
      constrain $ nv S..== lv

  refAbstract :: String -> FloatCons -> EDGMonad (Ref Float)
  refAbstract name' c
    | unSAT c = throw $ "No valid solution for float " ++ show name' ++ "."
    | otherwise = do
      n <- ref name'
      returnAnd n $ case c of
        FCBottom -> return ()
        FCOneOf (OneOf m) -> do
          nv <- sbv n
          lvs <- mapM lit (Set.toList m)
          case lvs of
            [] -> throw $ "Contraints for float `" ++ show n ++ "` insoluble."
            ts -> constrain $ S.bAny ((S..==) nv) ts
        FCRange (Range lb ub) -> do
          case lb of
            Nothing -> return ()
            Just (LowerBound inc val) -> do
              nv <- sbv n
              lv <- lit val
              case inc of
                Inclusive    -> constrain $ lv S..<= nv
                NonInclusive -> constrain $ lv S..<  nv
                _            -> error "This should never happen"
          case ub of
            Nothing -> return ()
            Just (UpperBound inc val) -> do
              nv <- sbv n
              lv <- lit val
              case inc of
                Inclusive    -> constrain $ lv S..>= nv
                NonInclusive -> constrain $ lv S..>  nv
                _            -> error "This should never happen"

  sbv :: Ref Float -> SBVMonad (SBV Float)
  sbv r = do
    val <- uses @SBVS floatRef (Map.lookup r)
    case val of
      Nothing -> throw $ "No ref to float `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v

  lit :: Float     -> SBVMonad (SBV Float)
  lit = return . S.literal

  add :: Ref Float -> SBV Float -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS floatRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to float `" ++ show r ++ "` already exists."
      False -> floatRef @SBVS %= (Map.insert r s)

  getName :: Ref Float -> String
  getName = unpack

instance InvertSBV Float where

  extract :: Modelable a => DecodeState -> a -> Ref Float -> Maybe Float
  extract _ model (Ref name) = getModelValue name model

instance EDGEquals Float
instance EDGOrd Float
