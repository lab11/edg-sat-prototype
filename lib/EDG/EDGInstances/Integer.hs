

module EDG.EDGInstances.Integer where

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
import EDG.EDGDatatype
import EDG.EDGInstances.Bool

instance SBVAble Integer where

  type SBVType Integer = SBV Integer

  type RefType Integer = Ref Integer

  ref :: String -> EDGMonad (Ref Integer)
  ref name = let n = Ref name in errContext context $
      returnAnd n (errContext context $ sbv n)
    where
      context = "(ref :: Integer) `" ++ name ++ "`"

  isAbstract :: IntCons -> SBV Integer -> SBVMonad (SBV Bool)
  isAbstract ICBottom _ = return $ S.literal True
  isAbstract (ICOneOf m) s = errContext context $ do
    return $ case lvs of
      [] -> S.literal False
      ts -> S.bAny ((S..==) s) ts
    where
      lvs = map S.literal (Set.toList . unpack $ m)
      context = "(isAbstract :: Integer) `" ++ show m ++ "` `" ++ show s ++ "`"
  isAbstract ICOther{..} s = errContext context $ do
    n <- noCons
    let l = lbCons
        u = ubCons
    return $ S.bAnd [n,l,u]
    where
      noCons = case none of
        Nothing -> return $ S.literal True
        Just m -> let lvs = map S.literal (Set.toList . unpack $ m) in
          return $ case lvs of
            [] -> S.literal True
            ts -> S.bAll ((S../=) s) ts
      lbCons = case lower of
        Nothing -> S.literal True
        Just (LowerBound Inclusive    val) -> (S.literal val) S..<= s
        Just (LowerBound NonInclusive val) -> (S.literal val) S..<  s
        _ -> error "Impossible State"
      ubCons = case upper of
        Nothing -> S.literal True
        Just (UpperBound Inclusive    val) -> (S.literal val) S..>= s
        Just (UpperBound NonInclusive val) -> (S.literal val) S..>  s
        _ -> error "Impossible State"
      context = "(isAbstract :: Integer) `" ++ show s ++ "'"

  sbv :: Ref Integer -> SBVMonad (SBV Integer)
  sbv r = errContext ("SBV:" ++ context) $ do
    val <- uses @SBVS integerRef (Map.lookup r)
    case val of
      Just v  -> return v
      Nothing -> do
        s <- sInteger . unpack $ r
        add r s
        return s
    where
      context = "(sbv :: Integer) `" ++ show r ++ "`"

  lit :: Integer     -> SBVMonad (SBV Integer)
  lit l = errContext context $ return . S.literal $ l
    where
      context = "(lit :: Integer) `" ++ show l ++ "`"

  add :: Ref Integer -> SBV Integer -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS integerRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to integer `" ++ show r ++ "` already exists."
      False -> integerRef @SBVS %= (Map.insert r s)
    where
      context = "(add :: Integer) `" ++ show r ++ "` `" ++ show s ++ "`"

  getName :: Ref Integer -> String
  getName = unpack

instance InvertSBV Integer where

  extract :: Modelable a => DecodeState -> a -> Ref Integer -> Maybe Integer
  extract _ model (Ref name) = getModelValue name model

instance EDGEquals Integer
instance EDGOrd Integer
instance EDGNum Integer
