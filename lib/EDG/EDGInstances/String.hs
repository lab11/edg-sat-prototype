
module EDG.EDGInstances.String where

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



instance SBVAble String where

  type SBVType String = SBV String

  type RefType String = Ref String

  ref :: String -> EDGMonad (Ref String)
  ref name = let n = Ref name in returnAnd n (sString name >>= add n)

  refConcrete :: String -> String -> EDGMonad (Ref String)
  refConcrete name' s = do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      lv <- lit s
      constrain $ nv S..== lv

  refAbstract :: String -> StringCons -> EDGMonad (Ref String)
  refAbstract name' c
    | unSAT c = throw $ "No valid solution for string " ++ show name' ++ "."
    | otherwise = do
      n <- ref name'
      returnAnd n $ case c of
        SCBottom -> return ()
        (SCOneOf (OneOf m)) -> do
          nv  <- sbv n
          lvs <- mapM lit (Set.toList m)
          case lvs of
            [] -> throw $ "Constraints for string `" ++ show name' ++ "` insoluble."
            ts -> constrain $ S.bAny ((S..==) nv) ts
        (SCNoneOf (NoneOf m)) -> do
          nv <- sbv n
          lvs <- mapM lit (Set.toList m)
          case lvs of
            [] -> return ()
            ts -> constrain $ S.bAll ((S../=) nv) ts

  sbv :: Ref String -> SBVMonad (SBV String)
  sbv r = do
    val <- uses @SBVS stringRef (Map.lookup r)
    case val of
      Nothing -> throw $ "No ref to string `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v

  lit :: String     -> SBVMonad (SBV String)
  lit str = do
    empty <- uses @SBVS stringDecode (Bimap.null)
    case empty of
      -- If the Bimap is empty just insert with an index of 0
      True -> (stringDecode @SBVS %= Bimap.insert 0 str) >> (return . reWrap $ S.literal 0)
      False -> do
        exists <- uses @SBVS stringDecode (Bimap.lookupR str)
        case exists of
          -- If the string is already in the map, just return the integer we're
          -- already using for it.
          Just i -> return . reWrap @String $ S.literal i
          -- Otherwise allocate a new ID, add that pair to the Bimap, and
          -- return the new ID.
          Nothing -> do
            (max,_) <- uses @SBVS stringDecode (Bimap.findMax)
            let new = max + 1
            stringDecode @SBVS %= (Bimap.insert new str)
            return . reWrap @String $ S.literal new

  add :: Ref String -> SBV String -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS stringRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to string `" ++ show r ++ "` already exists."
      False -> stringRef @SBVS %= (Map.insert r s)

  getName :: Ref String -> String
  getName = unpack

instance InvertSBV String where


  -- TODO :: Change the entire implementation so that you can decode with only
  --         the gather state. This means that all of those literal invocations
  --         have to have a GatherMonad component that saves all the strings
  --         before going to the latter SBVMonad component.
  --
  --         That is however a non-trivial change, and we have temporary
  --         workaround. So we'll see what we can do about that.
  --
  extract :: Modelable a => DecodeState -> a -> Ref String -> Maybe String
  extract (getStringDecode -> m) model (Ref name) = do
    int <- getModelValue name model
    Bimap.lookup int m

instance EDGEquals String
