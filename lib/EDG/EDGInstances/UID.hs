
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
import EDG.EDGDatatype
import EDG.EDGInstances.Bool
import EDG.SBVWrap

-- import Debug.Trace

instance SBVAble UID' where

  type SBVType UID' = SBV UID'

  type RefType UID' = Ref UID'

  ref :: String -> EDGMonad (Ref UID')
  ref name = let n = Ref name in ec $ returnAnd n (ec $ sbv n)
    where
      ec :: (NamedMonad m, MonadExcept String m) => m a -> m a
      ec = errContext $ "(ref :: UID') `" ++ name ++ "`"

  isAbstract :: UIDCons -> SBV UID' -> SBVMonad (SBV Bool)
  isAbstract UCTop     _ = return $ S.literal False
  isAbstract UCBottom  _ = return $ S.literal True
  isAbstract (UCVal v) s = isConcrete v s
  isAbstract UCNew     _ = error $ "Unfixed UCNew slipped through "
    ++ "to the second pass"

  refAbstract :: String -> UIDCons -> EDGMonad (Ref UID')
  refAbstract name' c
    | trace ("getting ref for UID `" ++ name' ++ "` with cons `"
        ++ show c ++ "`") False = undefined
    | UCNew    <- c = fixAbstract UCNew >>= refAbstract name'
    | otherwise = defaultRefAbstract name' c

  sbv :: Ref UID' -> SBVMonad (SBV UID')
  sbv r = do
    val <- uses @SBVS uidRef (Map.lookup r)
    case val of
      Just v  -> return v
      Nothing -> do
        s <- sUID' . unpack $ r
        add r s
        return s

  lit :: UID'     -> SBVMonad (SBV UID')
  lit = return . reWrap . S.literal . unpack

  add :: Ref UID' -> SBV UID' -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS uidRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to UID' `" ++ show r ++ "` already exists."
      False -> uidRef @SBVS %= (Map.insert r s)

  getName :: Ref UID' -> String
  getName = unpack

  fixAbstract :: UIDCons -> EDGMonad UIDCons
  fixAbstract UCNew = UCVal . UID' <$> newUID
  fixAbstract a     = return a

instance InvertSBV UID' where

  extract :: Modelable a => DecodeState -> a -> Ref UID' -> Maybe UID'
  extract _ model (Ref name) = pack <$> getModelValue name model

instance EDGEquals UID'
