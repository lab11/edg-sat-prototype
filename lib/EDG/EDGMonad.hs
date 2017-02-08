
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
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Control.Newtype

import Control.Monad.Ether.Implicit
import Control.Monad.MonadSymbolic
import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..)
  , Modelable(..)
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
  -- Counter for assigning UIDs to things
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
    ssBoolRef   :: Map (Ref Bool)   (SBV Bool)
  , ssStringRef :: Map (Ref String) (SBV Integer)
  , ssFloatRef  :: Map (Ref Float)  (SBV Float)
  -- Map for assigning strings to integer values, so that they can be search
  , ssStringDecode :: Bimap Integer String
  } deriving (Show)

makeLensesWith abbreviatedFields ''SBVState

type SBVS = SBVState

-- | Transform the ending state from the Gather pass into the start state for
--   the SBV pass.
transformState :: GatherState -> SBVState
transformState _ = SBVState {
    ssBoolRef = Map.empty
  , ssStringRef = Map.empty
  , ssFloatRef = Map.empty
  , ssStringDecode = Bimap.empty
  }

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

  -- | Gets the name out of the Ref, mostly just internal.
  getName :: RefType t -> String

-- | Given an ambiguous value, return the corresponding Reference, throwing
--   an error if the value is unsatisfiable.
refAmbiguous :: SBVAble t => String -> Ambiguous t -> EDGMonad (RefType t)
refAmbiguous name Impossible = throw $ "Ambiguous Value \"" ++ name ++ "\" is unsatisfiable."
refAmbiguous name (Concrete v) = refConcrete name v
refAmbiguous name (Abstract c)
  | unSAT c   = throw $ "Ambiguous Value \"" ++ name ++ "\" is unsatisfiable."
  | otherwise = refAbstract name c

-- | Can we, given a reference to a particular element in a SatModel to
--   retrieve, retrieve it? Well, if we have the particular context, which
--   is the final combination of the gatherState and the SBV State
--
--   TODO :: Now I just need to fogiure out how I can get the SBVState back
--           out :V probably some shenanigans with MVars and whatnot. Whatever,
--           I'll deal with it later.
--
class SBVAble t => InvertSBV t where
  extract :: Modelable a => DecodeState -> a -> RefType t -> Maybe t

-- | This is final output we use to gether information we need to reconstruct
--   a design.
--
--   TODO :: Convert this from a type alias to an actual type of its own, and
--           make the other bits less vacuous.
type DecodeState = (GatherState,SBVState)

-- | Use the final GatherState and SBVState to generate a DecodeState that we
--   can use to reconstruct the design.
buildDecodeState :: GatherState -> SBVState -> DecodeState
buildDecodeState = (,)

-- | Get the string Decoder from the decodeState
getStringDecode :: DecodeState -> Bimap Integer String
getStringDecode d = d ^. _2 . stringDecode

-- | Get an equality constraint
class (SBVAble t,SBVAble Bool) => EDGEquals t where
  -- | Given a name for the new variable, get the predicate that asserts two
  --   elements are equal.
  equalE   :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  -- | As you'd expect.
  unequalE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)

-- | Same as `equalE` but chooses its own name, usually just something
--   pretty obvious.
(.==)   :: EDGEquals t => RefType t -> RefType t            -> EDGMonad (RefType Bool)
(.==) a b = equalE a b ("equalE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `unequalE` but chooses its own name, usually just something
--   pretty obvious.
(./=)   :: EDGEquals t => RefType t -> RefType t            -> EDGMonad (RefType Bool)
(./=) a b = unequalE a b ("unequalE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | And some constraints for boolean operators.
class (SBVAble t, SBVAble Bool) => EDGLogic t where
  notE     :: RefType t ->              String -> EDGMonad (RefType Bool)
  andE     :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  orE      :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  impliesE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)

-- | Same as `notE` but chooses its own name, usually just something
--   pretty obvious.
notE'    :: EDGLogic t => RefType t            -> EDGMonad (RefType Bool)
notE' a = notE a ("notE (" ++ getName a ++ ")")

-- | Same as `andE` but chooses its own name, usually just something
--   pretty obvious.
(.&&)    :: EDGLogic t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.&&) a b = andE a b ("andE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `orE` but chooses its own name, usually just something
--   pretty obvious.
(.||)    :: EDGLogic t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.||) a b = andE a b ("orE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `impliesE` but chooses its own name, usually just something
--   pretty obvious.
(.=>)    :: EDGLogic t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.=>) a b = impliesE a b ("impliesE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | And some constraints for ordered values
class (SBVAble t, SBVAble Bool) => EDGOrd t where
  gtE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  gteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  ltE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  lteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)

-- | Same as `ltE` but chooses its own name, usually just something
--   pretty obvious.
(.<)    :: EDGOrd t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.<) a b = ltE a b ("ltE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `lteE` but chooses its own name, usually just something
--   pretty obvious.
(.<=)    :: EDGOrd t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.<=) a b = lteE a b ("lteE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `gtE` but chooses its own name, usually just something
--   pretty obvious.
(.>)    :: EDGOrd t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.>) a b = gtE a b ("gtE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `gteE` but chooses its own name, usually just something
--   pretty obvious.
(.>=)    :: EDGOrd t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
(.>=) a b = gteE a b ("gteE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | And some constraints for ordered values
class (SBVAble t, SBVAble Bool) => EDGPartialOrd t where
  leqE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)

-- | Same as `gteE` but chooses its own name, usually just something
--   pretty obvious.
leqE' :: EDGPartialOrd t => RefType t -> RefType t           -> EDGMonad (RefType Bool)
leqE' a b = leqE a b ("leqE (" ++ getName a ++ ") (" ++ getName b ++ ")")
