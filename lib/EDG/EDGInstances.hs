
-- | The catch-all place to keep the many isntance declarations that go with
--   the EDGMonad classes.
--
--   TODO :: Reorganize the whole thing to make sure file structure is vaguely
--           sensible. Each major type should probably get its own module
--           with the central one collecting and re-exportin everything
--
module EDG.EDGInstances where

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

instance SBVAble Bool where
  type SBVType Bool = SBV Bool
  type RefType Bool = Ref Bool

  -- TODO :: see if we really need to keep track of the names in GatherMonad
  --         so that we can assign them UIDs or something.
  ref :: String -> EDGMonad (Ref Bool)
  ref name = let n = Ref name in returnAnd n (sBool name >>= add n)

  refConcrete :: String -> Bool -> EDGMonad (Ref Bool)
  refConcrete name' v = do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      lv <- lit v
      -- And now we assert that the value we created with `ref` has the
      -- correct concrete result.
      ec <- nv `sEquals` lv
      constrain ec

  refAbstract :: String -> BoolCons -> EDGMonad (Ref Bool)
  refAbstract name' (BoolCons (OneOf s)) = do
    n <- ref name'
    returnAnd n $ do
      nv  <- sbv n
      lvs <- mapM lit (Set.toList s)
      case lvs of
        [] -> throw $ "Constraints for bool `" ++ show name' ++ "` insoluble."
        (ts) -> constrain $ S.bAny ((S..==) nv) lvs

  sbv :: Ref Bool -> SBVMonad (SBV Bool)
  sbv r = do
    val <- uses @SBVS boolRef (\ m -> trace (show m) $ Map.lookup r m)
    case val of
      Nothing -> throw $ "No ref to bool `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v

  lit :: Bool -> SBVMonad (SBV Bool)
  lit = return . S.literal

  add :: Ref Bool -> SBV Bool -> SBVMonad ()
  add r s = do
    exists <- uses @SBVS boolRef (Map.member r)
    case exists of
      True  -> throw $ "Reference to Bool `" ++ show r ++ "` already exists."
      False -> boolRef @SBVS %= (Map.insert r s)



