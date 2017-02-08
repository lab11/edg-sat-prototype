
-- | Mostly the datatypes needed for the EDG Monad and the classes that let us
--   do more interesting stuff with them. Honestly this is suboptimal.
--
--   TODO :: Reorganize this so that the classes are agnostic of underlying
--           EDGMonad type, and most the classes into their own file.
--
module EDG.EDGMonad where

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

import EDG.Library.Types
import EDG.Predicates
import Algebra.Constrainable
import Algebra.AsPredicate

-- import Debug.Trace
trace _ b = b

-- | Tagged type we'll be using as references that can cross the Gather/SBV
--   boundary.
newtype Ref a = Ref {unRef :: String}
  deriving (Show, Read, Eq, Ord)

instance Newtype (Ref a) String where
  pack = Ref
  unpack = unRef

-- | The first pass monad, where we gather instructions on how to build the
--   SMT problem.
type GatherMonad = StateT GatherState (ExceptT String  Identity)

-- | The state datatype with all the various pieces of info we care about.
data GatherState = GatherState {
    gsUidCounter :: Integer
  } deriving (Show,Read)

-- Sigh, this TH splice has to come after all the types used in the datatype
-- and before any uses of the relevant
makeLensesWith abbreviatedFields ''GatherState

type GS = GatherState

-- | The starting state we're going to use when constructing an EDG problem.
initialGatherState :: GatherState
initialGatherState = GatherState {
    gsUidCounter = 0
  }

-- | The monad we use for generating the SMT problem, should be the standard
--   Symbolic Monad wrapped in a State that allows us to access the various
--   global information needed to build the actual design.
type SBVMonad = StateT SBVState (ExceptT String Symbolic)

data SBVState = SBVState {
    ssBoolRef :: Map (Ref Bool) (SBV Bool)
  } deriving (Show)

makeLensesWith abbreviatedFields ''SBVState

type SBVS = SBVState

-- | This monad lets us construct the design in a nice recursive fashion while
--   writing a list of instructions into the SBVMonad about how to actually
--   build the final SMT problem.
type EDGMonad = ScribeT SBVMonad GatherMonad



-- | Get a newUID and increment the counter.
newUID :: EDGMonad Integer
newUID = uidCounter @(GatherState) <+= 1

-- | The class for contrianable types that can be written as an element in an
--   SMT problem. Mainly gives us a way to construct an SBV representation of
--   the particular type.
class Constrainable t => SBVAble t where

  -- | The type of the particular variable in SBV land, in a way that allows us
  --   to get to the particular `SBV _` of the components making it up.
  type SBVType t = i | i -> t

  -- | This is the type of the ID that can be generated in GatherMonad and be
  --   used to retrieve an SBVType in the SBVMonad. This can actually be
  --   pretty complex, especially for Records and the like.
  type RefType t = j | j -> t

  -- | Given a name, gets you the external reference for an SBV value of that
  --   type, and the setup code in the SBVMonad
  ref :: String -> EDGMonad (RefType t)

  -- | Given a concrete value, it'll give you the corresponding reference and
  --   make sure the SBV tool things it's the right value.
  --
  --   This doesn't just return `(Reftype t, Ref Bool)` since an error or
  --   unsatisfiable constraint in an unused component is itself an error.
  --   Likewise for `refAbstract`. A useless component should be an error on
  --   its own, albeit one that should be caught when we're assembling a
  --   component.
  --
  --   TODO :: Reconsider the above decision.
  refConcrete :: String -> t -> EDGMonad (RefType t)

  -- | Given a set of constraints over a value, it'll give you the
  --   corresponding reference and make sure SBV knows that the reference
  --   should be decently constrained.
  refAbstract :: String -> Constraints t -> EDGMonad (RefType t)

  -- | Will retrive the SBV elem given a reference
  sbv :: RefType t -> SBVMonad (SBVType t)

  -- | Will convert a literal into the corresponding SBVType
  lit :: t -> SBVMonad (SBVType t)

  -- | Sets the stored internal ref map to the correct value, basically
  --   just for internal use. Don't push too hard with this one.
  add :: RefType t -> SBVType t -> SBVMonad ()


-- | Given an ambiguous value, return the corresponding Reference, throwing
--   an error if the value is unsatisfiable.
refAmbiguous :: SBVAble t => String -> Ambiguous t -> EDGMonad (RefType t)
refAmbiguous name Impossible = throw $ "Ambiguous Value \"" ++ name ++ "\" is unsatisfiable."
refAmbiguous name (Concrete v) = refConcrete name v
refAmbiguous name (Abstract c)
  | unSAT c   = throw $ "Ambiguous Value \"" ++ name ++ "\" is unsatisfiable."
  | otherwise = refAbstract name c


