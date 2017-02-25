
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

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Library.Types
import EDG.Predicates
import EDG.SBVWrap
import EDG.EDGDatatype

import qualified Debug.Trace as T
trace _ b = b


-- | Kinds for values in the EDG context
type ValKind = Kind' RecEqClass

-- | Kinds for Records in the EDG context
type RecKind = Map String ValKind

-- | The first pass monad, where we gather instructions on how to build the
--   SMT problem.
type GatherMonad = StateT GatherState (ExceptT String  Identity)

-- | The state datatype with all the various pieces of info we care about.
data GatherState = GatherState {
  -- Counter for assigning UIDs to things
    gsUidCounter     :: Integer
  , gsValEqClassCounter :: ValEqClass
  , gsRecEqClassCounter :: RecEqClass
  -- For each value, stores information about it.
  , gsValInfo  :: Map (Ref Value) ValInfo
  -- TODO :: Yeah, I should find a better way to do this, and generally
  --         minimize the meccesary amount of updating.
  , gsRecInfo  :: Map (Ref Record) RecInfo
  -- For each equality class over a record stores the kind for each field.
  , gsRecordKinds :: Map RecEqClass RecKind
  -- Stores the integer representations of each string
  -- TODO :: Gather all the data for this in the correct spot.
  -- ,gsStringDecode :: Bimap Integer String
  } deriving (Show,Read)

-- Sigh, this TH splice has to come after all the types used in the datatype
-- and before any uses of the relevant
makeLensesWith abbreviatedFields ''GatherState

type GS = GatherState

-- | The starting state we're going to use when constructing an EDG problem.
initialGatherState :: GatherState
initialGatherState = GatherState {
    gsUidCounter     = 0
  , gsValEqClassCounter = pack 0
  , gsRecEqClassCounter = pack 0
  , gsValInfo     = Map.empty
  , gsRecInfo     = Map.empty
  , gsRecordKinds = Map.empty
  }

-- | The monad we use for generating the SMT problem, should be the standard
--   Symbolic Monad wrapped in a State that allows us to access the various
--   global information needed to build the actual design.
type SBVMonad = StateT SBVState (ExceptT String Symbolic)

data SBVState = SBVState {
  -- The grand store that we use to get the SBV values for a given reference
  -- Is basically useless outside of the actual Symbolic monad.
    ssBoolRef     :: Map (Ref Bool)    (SBV Bool)
  , ssStringRef   :: Map (Ref String)  (SBV String)
  , ssFloatRef    :: Map (Ref Float)   (SBV Float)
  , ssUidRef      :: Map (Ref UID')    (SBV UID')
  , ssIntegerRef  :: Map (Ref Integer) (SBV Integer)
  , ssValueRef    :: Map (Ref Value)   (ValueSBV)
  , ssRecordRef   :: Map (Ref Record)  (RecSBV)
  -- Information to get the kinds of values
  , ssValInfo     :: Map (Ref Value)  ValInfo
  , ssRecInfo     :: Map (Ref Record) RecInfo
  , ssRecordKinds :: Map RecEqClass RecKind
  -- Map for assigning strings to integer values, so that they can be search
  , ssStringDecode :: Bimap Integer String
  } deriving (Show)

makeLensesWith abbreviatedFields ''SBVState

type SBVS = SBVState

-- | Transform the ending state from the Gather pass into the start state for
--   the SBV pass.
transformState :: GatherState -> SBVState
transformState GatherState{..} = SBVState {
    ssBoolRef = Map.empty
  , ssStringRef = Map.empty
  , ssFloatRef = Map.empty
  , ssUidRef = Map.empty
  , ssIntegerRef = Map.empty
  , ssValueRef = Map.empty
  , ssRecordRef = Map.empty
  , ssValInfo = gsValInfo
  , ssRecInfo = gsRecInfo
  , ssRecordKinds = gsRecordKinds
  , ssStringDecode = Bimap.empty
  }

-- | This monad lets us construct the design in a nice recursive fashion while
--   writing a list of instructions into the SBVMonad about how to actually
--   build the final SMT problem.
type EDGMonad = ScribeT SBVMonad GatherMonad

class Monad m => NamedMonad m where
  monadName :: m String

instance NamedMonad EDGMonad where
  monadName = return "EDG"

instance NamedMonad GatherMonad where
  monadName = return "Gather"

instance NamedMonad SBVMonad where
  monadName = return "SBV"

-- | Get a newUID and increment the counter.
newUID :: EDGMonad Integer
newUID = uidCounter @(GatherState) <+= 1

-- | Get a new EqClassID and increment the counter.
newValEqClass :: EDGMonad ValEqClass
newValEqClass = do
  n <- uses @GS valEqClassCounter (pack . (+ 1) . unpack)
  valEqClassCounter @GS .= n
  return n

-- | Get a new EqClassID and increment the counter.
newRecEqClass :: EDGMonad RecEqClass
newRecEqClass = do
  n <- uses @GS recEqClassCounter  (pack . (+ 1) . unpack)
  recEqClassCounter @GS .= n
  return n

-- | Catch an expcetion and append a string to it, so that we can have better
--   knowledge of what's actually happening.
--
--   TODO :: Replace this form of error management with something that
--           uses GHC.Stack and errorWithCallStack.
--           Alternately something that uses
--           Language.Haskell.TH.Syntax.location with a custom error type.
errContext :: (NamedMonad m, MonadExcept String m) => String -> m a -> m a
errContext s e = do
  n <- monadName
  T.trace (n ++ ": " ++ s) $ catch e appendContext
  where
    appendContext = throw . unlines
      . (\ e -> ["In Context : " ++ s] ++ e )
      . map ("  " ++) . lines

-- | The class for contrianable types that can be written as an element in an
--   SMT problem. Mainly gives us a way to construct an SBV representation of
--   the particular type.
class (S.EqSymbolic (SBVType t)
  ,Constrainable t
  ,Show t
  ,Show (Constraints t)
  ,Show (SBVType t)
  ,Show (RefType t)) => SBVAble t where

  -- | The type of the particular variable in SBV land, in a way that allows us
  --   to get to the particular `SBV _` of the components making it up.
  type SBVType t = i | i -> t

  -- | This is the type of the ID that can be generated in GatherMonad and be
  --   used to retrieve an SBVType in the SBVMonad. This can actually be
  --   pretty complex, especially for Records and the like.
  type RefType t = j | j -> t

  -- | Given a name, gets you the external reference for an SBV value of that
  --   type, and the setup code in the SBVMonad. Generally only meaningful for
  --   non-recursive values that don't need global information to use.
  ref :: String -> EDGMonad (RefType t)

  -- | Will convert a literal into the corresponding SBVType. Again, mostly
  --   useful for types that have simple non-recursive literal representations.
  lit :: t -> SBVMonad (SBVType t)

  -- | Will retrive the SBV elem given a reference, if no such element exists
  --   will create a new one and return that.
  sbv :: RefType t -> SBVMonad (SBVType t)

  -- | Sets the stored internal ref map to the correct value, basically
  --   just for internal use. Don't push too hard with this one.
  add :: RefType t -> SBVType t -> SBVMonad ()

  -- | For a given value and a symbolic value will
  --   give you a symbolic Bool for if they match.
  --
  --   This is actually a pretty good implementation whenever a literal
  --   exists for this sort of element. There's not many instances where you'd
  --   need to change it, even for recursive SBVAble types.
  isConcrete :: t -> SBVType t -> SBVMonad (SBV Bool)
  default isConcrete :: t -> SBVType t -> SBVMonad (SBV Bool)
  isConcrete = defaultIsConcrete

  -- | Given a concrete value, it'll give you the corresponding reference and
  --   make sure the SBV tool things it's the right value.
  --
  --   This doesn't just return `(Reftype t, Ref Bool)` since an error or
  --   unsatisfiable constraint in an unused component is itself an error.
  --   Likewise for `refAbstract`. A useless component should be an error on
  --   its own, albeit one that should be caught when we're assembling a
  --   component.
  --
  --   NOTE :: This default is to be overridden when we need to keep track
  --           of some additional metadata or do some transformation to the
  --           constraints on the first pass.
  refConcrete :: String -> t -> EDGMonad (RefType t)
  default refConcrete :: String -> t -> EDGMonad (RefType t)
  refConcrete = defaultRefConcrete

  -- | For a given set of constraints, and a symbolic value, will give you
  --   a bool to ensure that the constraints are satisfied.
  isAbstract :: Constraints t -> SBVType t -> SBVMonad (SBV Bool)

  -- | Given a set of constraints over a value, it'll give you the
  --   corresponding reference and make sure SBV knows that the reference
  --   should be decently constrained.
  --
  --   NOTE :: This default is to be overridden when we need to keep track
  --           of some additional metadata or do some transformation to the
  --           constraints on the first pass.
  refAbstract :: String -> Constraints t -> EDGMonad (RefType t)
  default refAbstract :: String -> Constraints t -> EDGMonad (RefType t)
  refAbstract = defaultRefAbstract


  -- | Gets the name out of the Ref, mostly just internal.
  getName :: RefType t -> String
  default getName :: Newtype (RefType t) String => RefType t -> String
  getName = unpack

  -- | Takes the version of a type used by the library and converts it into
  --   one that can be used during the evaluation process. This is mostly for
  --   saving metadata, assigning UIDs and similar tasks.
  fixConcrete :: t -> EDGMonad t
  fixConcrete = return

  -- | Takes the version of a type used by the library and converts it into
  --   one that can be used during the evaluation process. This is mostly for
  --   saving metadata, assigning UIDs and similar tasks.
  fixAbstract :: Constraints t -> EDGMonad (Constraints t)
  fixAbstract = return

defaultIsConcrete :: SBVAble t => t -> SBVType t -> SBVMonad (SBV Bool)
defaultIsConcrete v s = errContext context $ do
  lv <- lit v
  return $ s S..== lv
  where
    context = "default is concrete `" ++ show v ++ "' `" ++ show s ++ "'"

defaultRefConcrete :: SBVAble t => String -> t -> EDGMonad (RefType t)
defaultRefConcrete name' v = errContext ("egd:" ++ context) $ do
  n <- ref name'
  returnAnd n $ errContext ("sbv:"++context) $ do
    nv <- sbv n
    constrain =<< isConcrete v nv
  where
    context = "defaultRefConcrete `" ++ name' ++ "` `" ++ show v ++ "`"

defaultRefAbstract :: SBVAble t
                   => String -> Constraints t -> EDGMonad (RefType t)
defaultRefAbstract name' c
  | unSAT c = error $ "Variable `" ++ name' ++ "` is unsatisfiable."
  | otherwise = errContext context $  do
    n <- ref name'
    returnAnd n $ do
      nv <- sbv n
      constrain =<< isAbstract c nv
  where
      context = "defaultRefAbstract `" ++ name' ++ "` `" ++ show c ++ "`"

-- | Given an ambiguous value, return the corresponding Reference, throwing
--   an error if the value is unsatisfiable.
refAmbiguous :: (SBVAble t, Show (Constraints t))
             => String -> Ambiguous t -> EDGMonad (RefType t)
refAmbiguous name Impossible = throw $ "Ambiguous Value \"" ++ name ++ "\" is unsatisfiable after collapse."
refAmbiguous name (Concrete v) = refConcrete name v
refAmbiguous name (Abstract c)
  | unSAT c   = throw $ "Ambiguous Value \"" ++ name ++ "\" with value \""
    ++ show c ++ "\" is unsatisfiable."
  | otherwise = refAbstract name c

-- | Given an ambiguous value, return the corresponding Reference, throwing
--   an error if the value is unsatisfiable.
fixAmbiguous :: SBVAble t => Ambiguous t -> EDGMonad (Ambiguous t)
fixAmbiguous Impossible   = return Impossible
fixAmbiguous (Concrete v) = Concrete <$> fixConcrete v
fixAmbiguous (Abstract c) = Abstract <$> fixAbstract c

-- | given an anbiguous value, and a symbolic value gives you a symbolic bool
--   for if they match.
isAmbiguous :: SBVAble t => Ambiguous t -> SBVType t -> SBVMonad (SBV Bool)
isAmbiguous Impossible   _ = return $ S.literal False
isAmbiguous (Concrete v) s = isConcrete v s
isAmbiguous (Abstract c) s = isAbstract c s


-- | Like the usual `sbv` but errors when a duplicate element is created.
--   Also take in a typename for error messages
--
--   TODO :: See if you can figure out what's causing multiple creation
--           instances and make sure it stops, then undo this hack.
sbvNoDup :: (SBVAble t, Show (RefType t), Ord (RefType t))
         => String
         -> Lens' SBVS (Map (RefType t) (SBVType t))
         -> RefType t -> SBVMonad (SBVType t)
sbvNoDup typeName lens ref = errContext context $ sbv ref
  -- do
  --   val <- uses @SBVS lens (Map.lookup ref)
  --   case val of
  --     Just v  -> do
  --       s <- get @SBVS
  --       throw $ "SBV Var `"++ show ref ++ "` of type " ++
  --         typeName ++ " already exists. \n\n" ++ show s
  --     Nothing -> sbv ref
  where
    context = "sbvNoDup `" ++ show ref ++ "` `" ++ typeName ++ "`"

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
getDSStringDecode :: DecodeState -> Bimap Integer String
getDSStringDecode d = d ^. _2 . stringDecode

-- | Get the map of value information from the decode state.
getDSValInfo :: DecodeState -> Map (Ref Value) ValInfo
getDSValInfo s = s ^. _2 . valInfo

-- | get the map of record information from the decode state
getDSRecInfo :: DecodeState -> Map (Ref Record) RecInfo
getDSRecInfo s = s ^. _2 . recInfo

-- | ease of se internal funtion that allow us to easily generate a binary
--   operator on refs from an operator on sbv values
mkBinOp :: (SBVAble i,SBVAble j, SBVAble k, S.EqSymbolic (SBVType k))
        => (SBVType i -> SBVType j -> SBVType k)
        -> String
        -> RefType i -> RefType j
        -> String
        -> EDGMonad (RefType k)
mkBinOp op opName a b name = errContext context $ do
  n <- ref name
  returnAnd n $ errContext context $ do
    av <- sbv a
    bv <- sbv b
    nv <- sbv n
    constrain $ nv S..== (op av bv)
  where
    context = opName ++ " `" ++ show a ++ "` `" ++ show b ++ "` `"
      ++ show name ++ "`"

-- | ease of se internal funtion that allow us to easily generate a unary
--   operator on refs from an operator on sbv values
mkUnOp :: (SBVAble j, SBVAble k, S.EqSymbolic (SBVType k))
       => (SBVType j -> SBVType k)
       -> String
       ->  RefType j
       ->  String
       ->  EDGMonad (RefType k)
mkUnOp op opName a name = errContext context $ do
  n <- ref name
  returnAnd n $ errContext context $ do
    av <- sbv a
    nv <- sbv n
    constrain $ nv S..== (op av)
  where
    context = opName ++ " `" ++ show a ++ "` `"
      ++ show name ++ "`"

-- | Get an equality constraint
class (SBVAble t,SBVAble Bool) => EDGEquals t where

  -- | Given a name for the new variable, get the predicate that asserts two
  --   elements are equal.
  equalE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default equalE :: (SBVType Bool ~ S.SBV Bool,S.EqSymbolic (SBVType t))
                 => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  equalE = mkBinOp (S..==) "equalE"

  -- | As you'd expect.
  unequalE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default unequalE :: (SBVType Bool ~ S.SBV Bool,S.EqSymbolic (SBVType t))
                   => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  unequalE = mkBinOp (S../=) "unequalE"

-- | Same as `equalE` but chooses its own name, usually just something
--   pretty obvious.
(.==)   :: EDGEquals t => RefType t -> RefType t            -> EDGMonad (RefType Bool)
(.==) a b = equalE a b ("equalE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `unequalE` but chooses its own name, usually just something
--   pretty obvious.
(./=)   :: EDGEquals t => RefType t -> RefType t            -> EDGMonad (RefType Bool)
(./=) a b = unequalE a b ("unequalE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | And some constraints for boolean operators.
--
--   We can't really add defaults to this, since the only sensible defaults
--   requre that we're working with Booleans. Other implementations would have
--   to be custom. So we just have a boolean implementation.
class (SBVAble t, SBVAble Bool) => EDGLogic t where
  notE     :: RefType t ->              String -> EDGMonad (RefType t)
  andE     :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  orE      :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  impliesE :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  nandE    :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  norE     :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  xorE     :: RefType t -> RefType t -> String -> EDGMonad (RefType t)


-- | Same as `notE` but chooses its own name, usually just something
--   pretty obvious.
notE'    :: EDGLogic t => RefType t -> EDGMonad (RefType t)
notE' a = notE a ("notE (" ++ getName a ++ ")")

-- | Same as `andE` but chooses its own name, usually just something
--   pretty obvious.
(.&&)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.&&) a b = andE a b ("andE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `orE` but chooses its own name, usually just something
--   pretty obvious.
(.||)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.||) a b = andE a b ("orE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `impliesE` but chooses its own name, usually just something
--   pretty obvious.
(.=>)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.=>) a b = impliesE a b ("impliesE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `nandE` but chooses its own name, usually just something
--   pretty obvious.
(.~&)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.~&) a b = nandE a b ("nandE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `norE` but chooses its own name, usually just something
--   pretty obvious.
(.~|)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.~|) a b = norE a b ("norE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | Same as `xorE` but chooses its own name, usually just something
--   pretty obvious.
(.<+>)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.<+>) a b = xorE a b ("xorE (" ++ getName a ++ ") (" ++ getName b ++ ")")

-- | And some constraints for ordered values
class (SBVAble t, SBVAble Bool) => EDGOrd t where

  gtE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default gtE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  gtE = mkBinOp (S..>) "gtE"

  gteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default gteE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  gteE = mkBinOp (S..>=) "gteE"

  ltE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default ltE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  ltE = mkBinOp (S..<) "ltE"

  lteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default lteE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  lteE = mkBinOp (S..>=) "lteE"

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
