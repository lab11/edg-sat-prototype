
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



instance SBVAble UID where

  type SBVType UID = SBV Integer

  type RefType UID = Ref UID

  ref :: String -> EDGMonad (Ref UID)
  ref name = undefined
  -- let n = Ref name in returnAnd n (sFloat name >>= add n)

  refConcrete :: String -> UID -> EDGMonad (Ref UID)
  refConcrete name' s = undefined
    -- do
    -- n <- ref name'
    -- returnAnd n $ do
    --   nv <- sbv n
    --   lv <- lit s
    --   constrain $ nv S..== lv

  refAbstract :: String -> UIDCons -> EDGMonad (Ref UID)
  refAbstract name' c = undefined
    -- | unSAT c = throw $ "No valid solution for float " ++ show name' ++ "."
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

  sbv :: Ref UID -> SBVMonad (SBV Integer)
  sbv r = undefined
    -- do
    -- val <- uses @SBVS floatRef (Map.lookup r)
    -- case val of
    --   Nothing -> throw $ "No ref to float `" ++ show r ++ "` found, cannot continue."
    --   Just v  -> return v

  lit :: UID     -> SBVMonad (SBV Integer)
  lit = undefined
    -- return . S.literal

  add :: Ref UID -> SBV Integer -> SBVMonad ()
  add r s = undefined
    -- do
    -- exists <- uses @SBVS floatRef (Map.member r)
    -- case exists of
    --   True  -> throw $ "Reference to float `" ++ show r ++ "` already exists."
    --   False -> floatRef @SBVS %= (Map.insert r s)

  getName :: Ref UID -> String
  getName = unpack

instance InvertSBV UID where

  extract :: Modelable a => DecodeState -> a -> Ref UID -> Maybe UID
  extract _ model (Ref name) = UIDNewtype <$> getModelValue name model

instance EDGEquals UID where

  equalE :: Ref UID -> Ref UID -> String -> EDGMonad (Ref Bool)
  equalE a b name = undefined
    -- do
    -- let n = Ref name
    -- returnAnd n $ do
    --   av <- sbv a
    --   bv <- sbv b
    --   add n (av S..== bv)

  unequalE :: Ref UID -> Ref UID -> String -> EDGMonad (Ref Bool)
  unequalE a b name = undefined
    -- do
    -- let n = Ref name
    -- returnAnd n $ do
    --   av <- sbv a
    --   bv <- sbv b
    --   add n (av S../= bv)

