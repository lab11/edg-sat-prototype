
-- | The catch-all place to keep the many isntance declarations that go with
--   the EDGMonad classes.
--
--   TODO :: Reorganize the whole thing to make sure file structure is vaguely
--           sensible. Each major type should probably get its own module
--           with the central one collecting and re-exportin everything
--
module EDG.EDGInstances (
  module EDG
) where

import qualified EDG.EDGInstances.Bool    as EDG
import qualified EDG.EDGInstances.String  as EDG
import qualified EDG.EDGInstances.Float   as EDG
import qualified EDG.EDGInstances.UID     as EDG
import qualified EDG.EDGInstances.Integer as EDG

import EDG.EDGMonad
import Control.Monad.MonadSymbolic
import Control.Monad.Scribe

-- | No other good place to keep this instance for now.
--
--   TODO :: Find better location for this instance
instance MonadConstrain EDGMonad (Ref Bool) where
  constrain s = returnAnd () $ sbv s >>= constrain

-- Default starting stuff for an EDGInstance :
--
--  import Data.EqMap (EqMap)
--  import qualified Data.EqMap as EqMap
--  import Data.Map (Map)
--  import qualified Data.Map as Map
--  import Data.Set (Set)
--  import qualified Data.Set as Set
--
--  import Control.Newtype
--
--  import Control.Monad.Ether.Implicit
--  import Control.Monad.MonadSymbolic
--  import Data.SBV (
--      Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
--    , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
--    )
--  import Data.SBV.Internals (
--    CWVal(..)
--    )
--  import qualified Data.SBV as S
--  import Control.Monad.Scribe
--  import Control.Monad.Identity (Identity)
--
--  import Control.Lens.Ether.Implicit
--  import Control.Lens.TH
--
--  import Algebra.PartialOrd
--  import Algebra.Lattice
--
--  import Algebra.Constrainable
--  import Algebra.AsPredicate
--
--  import EDG.Library.Types
--  import EDG.Predicates
--  import EDG.EDGMonad

--
--  class SBVAble String where
--
--    type SBVType String = SBV String
--
--    type RefType String = Ref String
--
--    ref :: String -> EDGMonad (Ref String)
--    ref = undefined
--
--    refConcrete :: String -> String -> EDGMonad (Ref String)
--    refConcrete = undefined
--
--    refAbstract :: String -> StringCons -> EDGMonad (Ref String)
--    refAbstract = undefined
--
--    sbv :: Ref String -> SBVMonad (SBV String)
--    sbv = undefined
--
--    lit :: String     -> SBVMonad (SBV String)
--    lit = undefined
--
--    add :: Ref String -> SBV String -> SBVMonad ()
--    add = undefined
--
--    getName :: Ref String -> String
--    getName = unpack
--
--  instance InvertSBV String where
--
--    extract :: Modelable a => DecodeState -> a -> Ref String -> Maybe String
--    extract = undefined
--
--  instance EDGEquals String where
--
--    equalE   :: Ref String -> Ref String -> String -> EDGMonad (Ref Bool)
--    equalE = undefined
--
--    unequalE :: Ref String -> Ref String -> String -> EDGMonad (Ref Bool)
--    unequalE = undefined


