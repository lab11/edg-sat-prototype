
module EDG.EDGInstances.Float where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import EDG.EDGDatatype
import EDG.EDGInstances.Bool



instance SBVAble Float where

  type SBVType Float = SBV Float

  type RefType Float = Ref Float

  ref :: String -> EDGMonad (Ref Float)
  ref name = let n = Ref name in ec $ returnAnd n (ec $ sbv n)
    where
      ec :: (NamedMonad m, MonadExcept String m) => m a -> m a
      ec = errContext $ "(ref :: String) `" ++ name ++ "`"

  isAbstract :: FloatCons -> SBV Float -> SBVMonad (SBV Bool)
  isAbstract FCBottom _ = return $ S.literal True
  isAbstract (FCOneOf (OneOf m)) s
    = return $ case lvs of
      [] -> S.literal False
      ts -> S.bAny ((S..==) s) ts
    where
      lvs = map S.literal (Set.toList m)
  isAbstract (FCRange (Range lb ub)) s = return $ lbCons S.&&& ubCons
    where
      lbCons = case lb of
        Nothing -> S.literal True
        Just (LowerBound Inclusive    val) -> (S.literal val) S..<= s
        Just (LowerBound NonInclusive val) -> (S.literal val) S..<  s
        _ -> error "Impossible State"
      ubCons = case ub of
        Nothing -> S.literal True
        Just (UpperBound Inclusive    val) -> (S.literal val) S..>= s
        Just (UpperBound NonInclusive val) -> (S.literal val) S..>  s
        _ -> error "Impossible State"

  sbv :: Ref Float -> SBVMonad (SBV Float)
  sbv r = do
    val <- uses @SBVS floatRef (Map.lookup r)
    case val of
      Just v  -> return v
      Nothing -> do
        s <- sFloat . unpack $ r
        add r s
        return s

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
instance EDGNum Float
