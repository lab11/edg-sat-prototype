{-# LANGUAGE UndecidableInstances #-}

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

import Control.Monad.Trans.Class
-- import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit
import Control.Monad.Ether.Implicit.Writer
import Control.Monad.Ether.Implicit.Reader
import Control.Monad.Ether.Implicit.Except
import Control.Monad.Ether.Implicit.State.Strict
import Control.Lens.TH

import GHC.Generics
import Control.DeepSeq

import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Expression
import EDG.Library.Types
import EDG.Predicates
import EDG.SBVWrap
import EDG.EDGDatatype
import EDG.PortTypes
import EDG.ElemTypes

import qualified Data.SBV.Internals as S
import qualified Data.SBV.Dynamic as S

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
  , gsValInfo  :: (Map (Ref Value) ValInfo)
  -- And the reverse lookup tables for values without a set class.
  , gsReverseValEq :: (Map ValEqClass (Set (Ref Value)))
  -- TODO :: Yeah, I should find a better way to do this, and generally
  --         minimize the meccesary amount of updating.
  , gsRecInfo  :: (Map (Ref Record) RecInfo)
  -- And reverse lookups for record equlity classes.
  , gsReverseRecEq :: (Map RecEqClass (Set (Ref Record)))
  -- Reverse lookup to find record kinds that contain other records
  -- I.E Key := some RecEqClass
  --     Val := all the RecEqClasses that point to Key
  , gsReverseRecKind :: (Map RecEqClass (Set RecEqClass))
  -- For each equality class over a record stores the kind for each field.
  , gsRecordKinds :: (Map RecEqClass RecKind)
  -- Storage for each major class of port, raw ones that don't come from a
  -- context of a module or link
  , gsBarePortInfo   :: (Map (Ref Port) (PortInfo Port))
  , gsLinkPortInfo   :: (Map (Ref LinkPort) (PortInfo LinkPort))
  , gsModulePortInfo :: (Map (Ref ModPort ) (PortInfo ModPort))
  -- And Linkwise for each class of element
  , gsLinkInfo       :: (Map (Ref Link  ) (ElemInfo Link   LinkPort))
  , gsModuleInfo     :: (Map (Ref Module) (ElemInfo Module ModPort ))
  -- Convinience Store for all the connection booleans that we're
  -- going to be using for allSat
  , gsConnectionVars :: (Set (Ref Bool))
  -- Stores the integer representations of each string
  -- TODO :: Gather all the data for this in the correct spot.
  -- ,gsStringDecode :: Bimap Integer String
  }

-- This is where we need undecidable instances, but ExpContext EDG is
-- unambiguous created later on. There's no real recursion or anything.
deriving instance (ExpContext EDG) => Show GatherState
deriving instance (ExpContext EDG) => Read GatherState
deriving instance () => Generic GatherState
deriving instance (ExpContext EDG, NFData (ExpValue EDG)
  ,NFData (ExpLiteral EDG)) => NFData GatherState

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
  , gsValInfo        = Map.empty
  , gsReverseValEq   = Map.empty
  , gsRecInfo        = Map.empty
  , gsReverseRecEq   = Map.empty
  , gsReverseRecKind = Map.empty
  , gsRecordKinds    = Map.empty
  , gsBarePortInfo   = Map.empty
  , gsLinkPortInfo   = Map.empty
  , gsModulePortInfo = Map.empty
  , gsLinkInfo       = Map.empty
  , gsModuleInfo     = Map.empty
  , gsConnectionVars = Set.empty
  }

-- | The monad we use for generating the SMT problem, should be the standard
--   Symbolic Monad wrapped in a State that allows us to access the various
--   global information needed to build the actual design.
type SBVMonad = StateT SBVState (ExceptT String Symbolic)
data SBVState = SBVState {
  -- The grand store that we use to get the SBV values for a given reference
  -- Is basically useless outside of the actual Symbolic monad.
    ssBoolRef     :: (Map (Ref Bool)    (SBV Bool))
  , ssStringRef   :: (Map (Ref String)  (SBV String))
  , ssFloatRef    :: (Map (Ref Float)   (SBV Float))
  , ssUidRef      :: (Map (Ref UID')    (SBV UID'))
  , ssIntegerRef  :: (Map (Ref Integer) (SBV Integer))
  , ssValueRef    :: (Map (Ref Value)   (ValueSBV))
  , ssRecordRef   :: (Map (Ref Record)  (RecSBV))
  -- Information to get the kinds of values
  , ssValInfo     :: (Map (Ref Value)  ValInfo)
  , ssRecInfo     :: (Map (Ref Record) RecInfo)
  , ssRecordKinds :: (Map RecEqClass RecKind)
  -- Map for assigning strings to integer values, so that they can be search
  , ssStringDecode :: (Bimap Integer String)
  } deriving (Show)

instance NFData SBVState where
  -- | We're explicitly not trying to evaluate each of the SBV variables
  --   here since that would defeat the purpose of reducing this to some
  --   normal form.
  rnf SBVState{..} = rnf @[_] [rnf ssValInfo, rnf ssRecInfo
    , rnf ssRecordKinds]

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

-- | This monad is what we use when extracting a solution from a particular
--   test result
--
-- TODO :: make the relevant typeclasses use this so that it's reasonable
type ExtractMonad a = ExceptT String (Reader (a,DecodeState))

instance NamedMonad EDGMonad where
  monadName = return "EDG    "

instance NamedMonad GatherMonad where
  monadName = return "Gather "

instance NamedMonad SBVMonad where
  monadName = return "SBV    "

instance NamedMonad (ExtractMonad a) where
  monadName = return "Extract"

-- | Get a newUID and increment the counter.
newUID :: EDGMonad Integer
newUID = uidCounter @(GatherState) <+= 1
{-# INLINE newUID #-}

-- | Get a wrapped new UID
newConcreteUID :: EDGMonad UID'
newConcreteUID = UID' <$> newUID
{-# INLINE newConcreteUID #-}

-- | Get a new EqClassID and increment the counter.
newValEqClass :: EDGMonad ValEqClass
newValEqClass = do
  n <- uses @GS valEqClassCounter (pack . (+ 1) . unpack)
  valEqClassCounter @GS .= n
  return n
{-# INLINE newValEqClass #-}

-- | Get a new EqClassID and increment the counter.
newRecEqClass :: EDGMonad RecEqClass
newRecEqClass = do
  n <- uses @GS recEqClassCounter  (pack . (+ 1) . unpack)
  recEqClassCounter @GS .= n
  return n
{-# INLINE newRecEqClass #-}

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
  -- NOTE :: This is big hammer of traces. This will print out every
  --         context string the system will ever write. It's kinda gigantic.
  -- T.trace (n ++ ": " ++ s) $ return ()
  catch e (appendContext n)
  where
    appendContext n = throw . unlines
      . (\ e -> [("In Context ("++n++") : ") ++ s] ++ e )
      . map ("  " ++) . lines
{-# INLINE errContext #-}

-- | throw an error when an operation that returns maybe fails.
maybeThrow :: (MonadExcept String m) => String -> Maybe a -> m a
maybeThrow s  Nothing = throw s
maybeThrow _ (Just v) = return v
{-# INLINE maybeThrow #-}

-- TODO :: Change typesig to following and reimplement when we make the
--         ExtractMonad changes.
maybeThrow' :: String -> Maybe a -> Maybe a
maybeThrow' _ = id
{-# INLINE maybeThrow' #-}


-- | The class for contrianable types that can be written as an element in an
--   SMT problem. Mainly gives us a way to construct an SBV representation of
--   the particular type.
class (S.EqSymbolic (SBVType t)
  ,Constrainable t
  ,Show t
  ,Show (Constraints t)
  ,Show (SBVType t)
  ,Show (RefType t)
  ) => SBVAble t where

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
  --
  --   If the value already exists, just return the already existing thing
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
    context = "defaultIsConcrete `" ++ show v ++ "' `" ++ show s ++ "'"

defaultRefConcrete :: SBVAble t => String -> t -> EDGMonad (RefType t)
defaultRefConcrete name' v = errContext context $ do
  n <- ref name'
  returnAnd n $ errContext context $ do
    nv <- sbv n
    constrain =<< isConcrete v nv
  where
    context = "defaultRefConcrete `" ++ name' ++ "` `" ++ show v ++ "`"

defaultRefAbstract :: SBVAble t
                   => String -> Constraints t -> EDGMonad (RefType t)
defaultRefAbstract name' c
  | unSAT c = error $ "Variable `" ++ name' ++ "` is unsatisfiable."
  | otherwise = errContext context $ do
    n <- ref name'
    returnAnd n $ errContext context $ do
      nv <- errContext "sbv" $ sbv n
      errContext "cons" (constrain =<< (errContext "abs" $ isAbstract c nv))
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
  | isBottom c = ref name
  | otherwise = refAbstract name c

-- | Given an ambiguous value, return the corresponding Reference, throwing
--   an error if the value is unsatisfiable.
fixAmbiguous :: SBVAble t => Ambiguous t -> EDGMonad (Ambiguous t)
fixAmbiguous Impossible   = trace "fixAmbigImp" $ return Impossible
fixAmbiguous (Concrete v) = Concrete <$> fixConcrete v
fixAmbiguous (Abstract c) = Abstract <$> fixAbstract c
{-# INLINE fixAmbiguous #-}

-- | given an anbiguous value, and a symbolic value gives you a symbolic bool
--   for if they match.
isAmbiguous :: SBVAble t => Ambiguous t -> SBVType t -> SBVMonad (SBV Bool)
isAmbiguous Impossible   _ = return $ S.literal False
isAmbiguous (Concrete v) s = isConcrete v s
isAmbiguous (Abstract c) s = isAbstract c s
{-# INLINE isAmbiguous #-}

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
data DecodeState = DecodeState {
  --   dsUidCounter     :: Integer
  -- , dsValEqClassCounter :: ValEqClass
  -- , dsRecEqClassCounter :: RecEqClass
  --dFor each value, stores information about it.
    dsValInfo  :: !(Map (Ref Value) ValInfo)
  --dTODO :: Yeah, I should find a better way to do this, and generally
  --d        minimize the meccesary amount of updating.
  , dsRecInfo  :: !(Map (Ref Record) RecInfo)
  --dFor each equality class over a record stores the kind for each field.
  --, dsRecordKinds :: !(Map RecEqClass RecKind)
  --dStorage for each major class of port, raw ones that don't come from a
  --dcontext of a module or link
  , dsBarePortInfo   :: (Map (Ref Port) (PortInfo Port))
  , dsLinkPortInfo   :: !(Map (Ref LinkPort) (PortInfo LinkPort))
  , dsModulePortInfo :: !(Map (Ref ModPort ) (PortInfo ModPort))
  --dAnd Linkwise for each class of element
  , dsLinkInfo       :: !(Map (Ref Link  ) (ElemInfo Link   LinkPort))
  , dsModuleInfo     :: !(Map (Ref Module) (ElemInfo Module ModPort ))
  --dConvinience Store for all the connection booleans that we're
  --dgoing to be using for allSat
  --, dsConnectionVars :: !(Set (Ref Bool))
  --dStores the integer representations of each string
  --dTODO :: Gather all the data for this in the correct spot.
  --d,gsStringDecode :: Bimap Integer String
  --dThe grand store that we use to get the SBV values for a given reference
  --dIs basically useless outside of the actual Symbolic monad.
  --   dsBoolRef     :: (Map (Ref Bool)    (SBV Bool))
  -- , dsStringRef   :: (Map (Ref String)  (SBV String))
  -- , dsFloatRef    :: (Map (Ref Float)   (SBV Float))
  -- , dsUidRef      :: (Map (Ref UID')    (SBV UID'))
  -- , dsIntegerRef  :: (Map (Ref Integer) (SBV Integer))
  -- , dsValueRef    :: (Map (Ref Value)   (ValueSBV))
  -- , dsRecordRef   :: (Map (Ref Record)  (RecSBV))
  --dInformation to get the kinds of values
  --, dsValInfo     :: (Map (Ref Value)  ValInfo)
  --, dsRecInfo     :: (Map (Ref Record) RecInfo)
  -- , dsRecordKinds :: (Map RecEqClass RecKind)
  --dMap for assigning strings to integer values, so that they can be search
  , dsStringDecode :: (Bimap Integer String)
  }

deriving instance (ExpContext EDG) => Eq   DecodeState
deriving instance (ExpContext EDG) => Show DecodeState
instance (ExpContext EDG, NFData (ExpValue EDG)
  ,NFData (ExpLiteral EDG)) => NFData DecodeState where
  rnf DecodeState{..} =
    rnf @[_] [
        rnf dsValInfo
      , rnf dsRecInfo
      , rnf dsBarePortInfo
      , rnf dsLinkPortInfo
      , rnf dsModulePortInfo
      , rnf dsLinkInfo
      , rnf dsModuleInfo
      ]






makeLensesWith abbreviatedFields ''DecodeState
-- | Use the final GatherState and SBVState to generate a DecodeState that we
--   can use to reconstruct the design.
buildDecodeState :: GatherState -> SBVState -> DecodeState
buildDecodeState GatherState{..} SBVState{..} = DecodeState{
    dsValInfo        = gsValInfo -- :: !(Map (Ref Value) ValInfo)
  , dsRecInfo        = gsRecInfo -- :: !(Map (Ref Record) RecInfo)
  , dsBarePortInfo   = gsBarePortInfo -- :: (Map (Ref Port) (PortInfo Port))
    -- :: !(Map (Ref LinkPort) (PortInfo LinkPort))
  , dsLinkPortInfo   = gsLinkPortInfo
    -- :: !(Map (Ref ModPort ) (PortInfo ModPort))
  , dsModulePortInfo = gsModulePortInfo
    -- :: !(Map (Ref Link  ) (ElemInfo Link   LinkPort))
  , dsLinkInfo       = gsLinkInfo
    --  :: !(Map (Ref Module) (ElemInfo Module ModPort ))
  , dsModuleInfo     = gsModuleInfo
  , dsStringDecode   = ssStringDecode -- :: (Bimap Integer String)
  }
{-# INLINE buildDecodeState #-}

-- | Get the string Decoder from the decodeState
getDSStringDecode :: DecodeState -> Bimap Integer String
getDSStringDecode = dsStringDecode
{-# INLINE getDSStringDecode #-}

-- | Get the map of value information from the decode state.
getDSValInfo :: DecodeState -> Map (Ref Value) ValInfo
getDSValInfo = dsValInfo
{-# INLINE getDSValInfo #-}

-- | get the map of record information from the decode state
getDSRecInfo :: DecodeState -> Map (Ref Record) RecInfo
getDSRecInfo = dsRecInfo
{-# INLINE getDSRecInfo #-}

getDSBarePortInfo :: DecodeState -> Map (Ref Port) (PortInfo Port)
getDSBarePortInfo = dsBarePortInfo
{-# INLINE getDSBarePortInfo #-}

getDSModulePortInfo :: DecodeState -> Map (Ref ModPort) (PortInfo ModPort)
getDSModulePortInfo = dsModulePortInfo
{-# INLINE getDSModulePortInfo #-}

getDSLinkPortInfo :: DecodeState -> Map (Ref LinkPort) (PortInfo LinkPort)
getDSLinkPortInfo = dsLinkPortInfo
{-# INLINE getDSLinkPortInfo #-}

getDSModuleInfo :: DecodeState -> Map (Ref Module) (ElemInfo Module ModPort)
getDSModuleInfo = dsModuleInfo
{-# INLINE getDSModuleInfo #-}

getDSLinkInfo :: DecodeState -> Map (Ref Link) (ElemInfo Link LinkPort)
getDSLinkInfo = dsLinkInfo
{-# INLINE getDSLinkInfo #-}

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
{-# INLINE mkBinOp #-}

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
{-# INLINE mkUnOp #-}

-- | Get an equality constraint
class (SBVAble t,SBVAble Bool) => EDGEquals t where

  -- | Given a name for the new variable, get the predicate that asserts two
  --   elements are equal.
  equalE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default equalE :: (SBVType Bool ~ S.SBV Bool,S.EqSymbolic (SBVType t))
                 => RefType t -> RefType t -> String
                 -> EDGMonad (RefType Bool)
  equalE = mkBinOp (S..==) "equalE"

  -- | As you'd expect.
  unequalE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default unequalE :: (SBVType Bool ~ S.SBV Bool,S.EqSymbolic (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType Bool)
  unequalE = mkBinOp (S../=) "unequalE"

-- | Same as `equalE` but chooses its own name, usually just something
--   pretty obvious.
(.==)   :: EDGEquals t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(.==) a b = equalE a b ("equalE (" ++ getName a ++ ") (" ++ getName b ++ ")")
{-# INLINE (.==) #-}

-- | Same as `unequalE` but chooses its own name, usually just something
--   pretty obvious.
(./=)   :: EDGEquals t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(./=) a b = unequalE a b ("unequalE (" ++ getName a ++ ") ("
            ++ getName b ++ ")")
{-# INLINE (./=) #-}

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
notE' a = notE a ("Not $" ++ getName a ++ "")
{-# INLINE notE' #-}

-- | Same as `andE` but chooses its own name, usually just something
--   pretty obvious.
(.&&)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.&&) a b = andE a b ("(" ++ getName a ++ "):&&(" ++ getName b ++ ")")
{-# INLINE (.&&) #-}

-- | Same as `orE` but chooses its own name, usually just something
--   pretty obvious.
(.||)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.||) a b = orE a b ("(" ++ getName a ++ "):||(" ++ getName b ++ ")")
{-# INLINE (.||) #-}

-- | Same as `impliesE` but chooses its own name, usually just something
--   pretty obvious.
(.=>)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.=>) a b = impliesE a b ("(" ++ getName a ++ "):=>("
            ++ getName b ++ ")")
{-# INLINE (.=>) #-}

-- | Same as `nandE` but chooses its own name, usually just something
--   pretty obvious.
(.~&)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.~&) a b = nandE a b ("(" ++ getName a ++ "):~&(" ++ getName b ++ ")")
{-# INLINE (.~&) #-}

-- | Same as `norE` but chooses its own name, usually just something
--   pretty obvious.
(.~|)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.~|) a b = norE a b ("(" ++ getName a ++ "):~|(" ++ getName b ++ ")")
{-# INLINE (.~|) #-}

-- | Same as `xorE` but chooses its own name, usually just something
--   pretty obvious.
(.<+>)    :: EDGLogic t => RefType t -> RefType t -> EDGMonad (RefType t)
(.<+>) a b = xorE a b ("(" ++ getName a ++ "):<+>(" ++ getName b ++ ")")
{-# INLINE (.<+>) #-}

class (SBVAble t) => EDGNum t where
  negateE :: RefType t ->              String -> EDGMonad (RefType t)
  default negateE :: (Num (SBVType t))
                   => RefType t -> String
                   -> EDGMonad (RefType t)
  negateE = mkUnOp negate "negateE"

  plusE     :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  default plusE :: (Num (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType t)
  plusE = mkBinOp (+) "plusE"

  minusE      :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  default minusE :: (Num (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType t)
  minusE = mkBinOp (-) "minusE"

  multE :: RefType t -> RefType t -> String -> EDGMonad (RefType t)
  default multE :: (Num (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType t)
  multE = mkBinOp (*) "multE"

negateE' :: EDGNum t => RefType t -> EDGMonad (RefType t)
negateE' a = negateE a ("Negate $" ++ getName a ++ ")")

(.+) :: EDGNum t => RefType t -> RefType t -> EDGMonad (RefType t)
(.+) a b = plusE a b ("(" ++ getName a ++ "):+(" ++ getName b ++ ")")
{-# INLINE (.+) #-}

(.-) :: EDGNum t => RefType t -> RefType t -> EDGMonad (RefType t)
(.-) a b = minusE a b ("(" ++ getName a ++ "):-(" ++ getName b ++ ")")
{-# INLINE (.-) #-}

(.*) :: EDGNum t => RefType t -> RefType t -> EDGMonad (RefType t)
(.*) a b = multE a b ("(" ++ getName a ++ "):*(" ++ getName b ++ ")")
{-# INLINE (.*) #-}

-- | And some constraints for ordered values
class (SBVAble t, SBVAble Bool) => EDGOrd t where

  gtE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default gtE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType Bool)
  gtE = mkBinOp (S..>) "gtE"

  gteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default gteE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType Bool)
  gteE = mkBinOp (S..>=) "gteE"

  ltE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default ltE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType Bool)
  ltE = mkBinOp (S..<) "ltE"

  lteE :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)
  default lteE :: (SBVType Bool ~ S.SBV Bool,S.OrdSymbolic (SBVType t))
                   => RefType t -> RefType t -> String
                   -> EDGMonad (RefType Bool)
  lteE = mkBinOp (S..>=) "lteE"

-- | Same as `ltE` but chooses its own name, usually just something
--   pretty obvious.
(.<)    :: EDGOrd t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(.<) a b = ltE a b ("(" ++ getName a ++ "):<(" ++ getName b ++ ")")
{-# INLINE (.<) #-}

-- | Same as `lteE` but chooses its own name, usually just something
--   pretty obvious.
(.<=)    :: EDGOrd t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(.<=) a b = lteE a b ("(" ++ getName a ++ "):<=(" ++ getName b ++ ")")
{-# INLINE (.<=) #-}

-- | Same as `gtE` but chooses its own name, usually just something
--   pretty obvious.
(.>)    :: EDGOrd t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(.>) a b = gtE a b ("(" ++ getName a ++ "):>(" ++ getName b ++ ")")
{-# INLINE (.>) #-}

-- | Same as `gteE` but chooses its own name, usually just something
--   pretty obvious.
(.>=)    :: EDGOrd t => RefType t -> RefType t -> EDGMonad (RefType Bool)
(.>=) a b = gteE a b ("(" ++ getName a ++ "):>=(" ++ getName b ++ ")")
{-# INLINE (.>=) #-}

-- | And some constraints for ordered values
class (SBVAble t, SBVAble Bool) => EDGPartialOrd t where
  leqE  :: RefType t -> RefType t -> String -> EDGMonad (RefType Bool)

-- | Same as `gteE` but chooses its own name, usually just something
--   pretty obvious.
leqE' :: EDGPartialOrd t => RefType t -> RefType t -> EDGMonad (RefType Bool)
leqE' a b = leqE a b ("leqE (" ++ getName a ++ ") (" ++ getName b ++ ")")
{-# INLINE leqE' #-}

-- NOTE :: A special instance that helps us detect when we try to constrain
--         our problem with a constant False. This is a bit problematic
--         since it forces the solution to be unsat.
instance {-# OVERLAPPING #-} MonadConstrain SBVMonad (SBV Bool) where
  constrain a = errContext context $ case (S.unSBV a) of
    (S.SVal _ (Left cw)) -> if
      | cw == S.falseCW -> throw @String $ "Setting a constraint of False,"
        ++ " this system is now unsatifiable."
      | cw == S.trueCW  -> return ()
      | otherwise -> throw @String "unreachable, probably"
    _ -> lift $ constrain a
    where
      context = "constrain `" ++ show a ++ "`"


