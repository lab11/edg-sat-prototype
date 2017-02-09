

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
import EDG.EDGInstances.Bool

instance SBVAble Integer where

  type SBVType Integer = SBV Integer

  type RefType Integer = Ref Integer

  ref :: String -> EDGMonad (Ref Integer)
  ref name = let n = Ref name in returnAnd n (sInteger name >>= add n)

  refConcrete :: String -> Integer -> EDGMonad (Ref Integer)
  refConcrete name' s = do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      lv <- lit s
      constrain $ nv S..== lv

  refAbstract :: String -> IntCons -> EDGMonad (Ref Integer)
  refAbstract name' c
    | unSAT c = throw $ "No valid solution for Integer " ++ show name' ++ "."
    | ICBottom    <- c = ref name'
    | ICOneOf m   <- c = do
      n <- ref name'
      returnAnd n $ do
        nv  <- sbv n
        lvs <- mapM lit (Set.toList . unpack $ m)
        case lvs of
          [] -> throw $ "Contraints for Integer `" ++ show n ++ "` insoluble."
          ts -> constrain $ S.bAny ((S..==) nv) ts
    | ICOther{..} <- c = do
      n <- ref name'
      returnAnd n $ do
        flip mapM_ none  undefined
        flip mapM_ lower undefined
        flip mapM_ upper undefined
    -- | otherwise = do
    --   n <- ref name'
    --   returnAnd n $ case c of
    --     FCBottom -> return ()
    --     FCOneOf (OneOf m) -> do
    --       nv <- sbv n
    --       lvs <- mapM lit (Set.toList m)
    --       case lvs of
    --         [] -> throw $ "Contraints for float `" ++ show n ++ "` insoluble."
    --         ts -> constrain $ S.bAny ((S..==) nv) ts
    --     FCRange (Range lb ub) -> do
    --       case lb of
    --         Nothing -> return ()
    --         Just (LowerBound inc val) -> do
    --           nv <- sbv n
    --           lv <- lit val
    --           case inc of
    --             Inclusive    -> constrain $ lv S..<= nv
    --             NonInclusive -> constrain $ lv S..<  nv
    --             _            -> error "This should never happen"
    --       case ub of
    --         Nothing -> return ()
    --         Just (UpperBound inc val) -> do
    --           nv <- sbv n
    --           lv <- lit val
    --           case inc of
    --             Inclusive    -> constrain $ lv S..>= nv
    --             NonInclusive -> constrain $ lv S..>  nv
    --             _            -> error "This should never happen"

  sbv :: Ref Integer -> SBVMonad (SBV Integer)
  sbv r = do
    val <- uses @SBVS integerRef (Map.lookup r)
    case val of
      Nothing -> throw $ "No ref to integer `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v

  lit :: Integer     -> SBVMonad (SBV Integer)
  lit = return . S.literal

  add :: Ref Integer -> SBV Integer -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS integerRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to integer `" ++ show r ++ "` already exists."
      False -> integerRef @SBVS %= (Map.insert r s)

  getName :: Ref Integer -> String
  getName = unpack

instance InvertSBV Integer where

  extract :: Modelable a => DecodeState -> a -> Ref Integer -> Maybe Integer
  extract _ model (Ref name) = getModelValue name model

instance EDGEquals Integer where

  equalE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  equalE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..== bv)

  unequalE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  unequalE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S../= bv)

instance EDGOrd Integer where

  ltE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  ltE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..< bv)

  lteE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  lteE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..<= bv)

  gtE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  gtE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..> bv)

  gteE :: Ref Integer -> Ref Integer -> String -> EDGMonad (Ref Bool)
  gteE a b name = do
    let n = Ref name
    returnAnd n $ do
      av <- sbv a
      bv <- sbv b
      add n (av S..>= bv)
