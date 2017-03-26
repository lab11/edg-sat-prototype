
module EDG.EDGInstances.Bool where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

-- Find better place to put this
preventConjunction :: Set (Ref Bool) -> SBVMonad ()
preventConjunction (Set.toList -> bls) = errContext context $ do
  terms <- mapM sbv bls
  constrain $ S.bnot (S.bAnd terms)
  where
    context = "preventConjunction `" ++ show bls ++ "`"


-- * Instances for Bool

instance SBVAble Bool where
  type SBVType Bool = SBV Bool
  type RefType Bool = Ref Bool

  -- TODO :: see if we really need to keep track of the names in GatherMonad
  --         so that we can assign them UIDs or something.
  ref :: String -> EDGMonad (Ref Bool)
  ref name =  errContext context $ do
    n <- newRef name
    returnAnd n (errContext context $ sbv n)
    where
      context = "(ref :: Bool) `" ++ name ++ "`"

  isAbstract :: BoolCons -> SBV Bool -> SBVMonad (SBV Bool)
  isAbstract (BoolCons (OneOf sb)) s = do
    return $ case lvs of
      []   -> S.literal False
      (ts) -> S.bAny ((S..==) s) lvs
    where
      lvs = map S.literal (Set.toList sb)

  sbv :: Ref Bool -> SBVMonad (SBV Bool)
  sbv r = do
    val <- uses @SBVS boolRef $ Map.lookup r
    case val of
      Just v  -> return v
      Nothing -> do
        name <- getRefSBV r
        s <- sBool name
        add r s
        return s


  lit :: Bool -> SBVMonad (SBV Bool)
  lit = return . S.literal

  add :: Ref Bool -> SBV Bool -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS boolRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to Bool `" ++ show r ++ "` already exists."
      False -> do
        boolRef @SBVS %= (Map.insert r s)

instance InvertSBV Bool where

  extract :: Modelable a => DecodeState -> a -> Ref Bool -> Maybe Bool
  extract ds model r = getModelValue (getRefDS ds r) model

instance EDGEquals Bool

instance EDGLogic Bool where
  notE :: Ref Bool ->             String -> EDGMonad (Ref Bool)
  notE = mkUnOp (S.bnot) "notE"

  andE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  andE = mkBinOp (&&&) "andE"

  orE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  orE = mkBinOp (|||) "orE"

  impliesE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  impliesE = mkBinOp (==>) "impliesE"

  nandE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  nandE = mkBinOp (~&) "nameE"

  norE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  norE = mkBinOp (~|) "norE"

  xorE :: Ref Bool -> Ref Bool -> String -> EDGMonad (Ref Bool)
  xorE = mkBinOp (<+>) "xorE"
