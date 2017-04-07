{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
  Module : EDG
  Description : Library Creation and Problem Evaluation for Device Generation
  Maintainer : rkr@berkeley.edu

  This is the only module needed to define Modules, Links, and Ports within a
  component library, assemble them into a problem description, and then run that
  problem description.
  All the various portions of the device generation toolkit that are needed for
  library authors and testers have been imported, renamed, and organized into a
  simpler end-user interface.
-}

module EDG (
    pattern IntV    -- NOTE :: This section covers the major datatypes we end
  , pattern IntC    --         up using insode the system. An overview like
  , pattern BoolV   --         the one from yesterday is probably a good idea.
  , pattern BoolC
  , pattern FloatV
  , pattern FloatC
  , pattern StringV
  , pattern StringC
  , pattern UID
  , pattern NewUID
  , pattern Record
  , unknown         -- NOTE :: Constraints, you can probably mentions these
  , oneOf           --         only insofar as to note that we're simplifying
  , noneOf          --         the space of possible predicates so that we can
  , greaterThan     --         reason over them effectively ourselves.
  , greaterThanEq
  , lessThan
  , lessThanEq
  , (+/-)
  , between
  , AmbigRec        -- NOTE :: Haskell types important to the implementation
  , AmbigVal        --         but irrelevant to the explanation.
  , (<:=)
  , pattern Lit     -- NOTE :: This is all just a wrapper around the notion of
  , pattern (:==)   --         expressions. There's no real point to working
  , pattern (:/=)   --         with this directly. When writing the
  , pattern (:&&)   --         explanations, just use a more obvious syntax
  , pattern (:||)   --         even if you have to make it up.
  , pattern (:~&)
  , pattern (:~|)
  , pattern (:<+>)
  , pattern (:=>)
  , pattern Not
  , pattern JustOne
  , pattern All
  , pattern Any
  , pattern (:<)
  , pattern (:<=)
  , pattern (:>)
  , pattern (:>=)
  , pattern (:+)
  , pattern (:-)
  , pattern (:*)
  , pattern Sum
  , pattern Mult
  , pattern Negate
  , pattern If
  , pattern Count
  , Module          -- NOTE :: More haskell types relevant to the
  , Link            --         implementation but not the explanation.
  , ModulePort
  , LinkPort
  , PortName
  , ResourceUse     -- NOTE :: Resource constraints, worth mentioning to show
  , pattern (:|=)   --         how these can solve common EE tasks, and how
  , Constrainable (..) --      they transform down into basic constraints.
  , IsElem (..)     -- NOTE :: None of the rest of this is particularly
  , IsPort (..)     --         relevant, mostly stuff for settings, alternate
  , appendIdent     --         names for functions, and whatnot.
  , updateType
  , IsBlock (..)
  , replaceSignature
  , EDGSettings (..)
  , defaultSettings
  , parseSettings
  , EDGLibrary (..)
  , synthesize
  , makeSynthFunc
  , synthesizeWithSettings
  , endDef
  , range           -- NOTE :: Range syntax sugar.
  , validRange
  , initRange
  , rContained
  , rSubset
  , rSuperset
  , rNotDisjoint
) where



import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- import Debug.Trace
import Control.Lens.Ether.Implicit hiding ((:<),(:>))
import Data.Maybe (fromJust)
import GHC.Exts (Item,IsList)
import Control.Newtype
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Exit
import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq
import GHC.Generics

import Options.Applicative
import Data.Semigroup ((<>))
import Debug.Trace

import Data.Time
import Control.Newtype.Util

import qualified Data.SBV as SBV
import Data.Char (toLower)
-- import qualified Data.SBV.Dynamic as SBV hiding (satWith)
-- import qualified Data.SBV.Internals as SBV hiding (satWith)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Pretty.Simple as T

import qualified Algebra.Lattice as A (
    bottom
  , BoundedJoinSemiLattice
  )
import qualified Algebra.AsPredicate as A (
    LiftablePredicate
  , AsPredicate
  , PredDom
  , liftPredicate
  )
import qualified Algebra.Constrainable as A (
    Ambiguous(..)
  )
import qualified EDG.Predicates as E (
    oneOf
  , OneOfConstraint
  , noneOf
  , NoneOfConstraint
  , lessThan
  , lessThanEq
  , LTConstraint
  , greaterThan
  , greaterThanEq
  , GTConstraint
  )
import qualified Control.Monad.MonadSymbolic as S (
    constrain
  , Symbolic
  , SBV
  )
import qualified EDG.Library.Types.TypeVal as E (
  )
import qualified EDG.Expression as E (
    Exp(..)
  , ExpContext
  , ExpValue
  , ExpLiteral
  )
import qualified EDG.EDGInstances as E (
    runEDGMonad
  )
import qualified EDG.EDGInstances.Bool as E (
    preventConjunction
  )
import qualified EDG.EDGDatatype as E (
    Ref(..)
  , Port
  , EDG
  , Link
  , Module
  , ModPort
  , LinkPort
  , Resource
  )
import qualified EDG.EDGMonad as E (
    SBVState
  , GatherState
  , DecodeState
  , buildDecodeState
  , EDGMonad
  , SBVMonad
  , connectionVars
  , extract
  )
import qualified Data.SBV as SBV (
    satWith
  , allSatWith
  , defaultSMTCfg
  , SMTConfig(..)
  , AllSatResult(..)
  , SMTResult(..)
  , SatResult(..)
  )
import qualified Data.IORef as IO (
    IORef
  , newIORef
  , readIORef
  )
import qualified EDG.ElemIncludes as E (
    pPrint
  )
import qualified EDG.PortTypes as E (
    convertPortState
  , PortM
  , PortDesc
  , PortValue(..)
  , pvNewInt
  , pvSetIdent
  , pvSetClass
  , pvSetType
  , pvUID
  , pvConnected
  , pvClass
  , pvConnectedTo
  , pvType
  , pvAddLiteral
  , runPortM
  )
import qualified EDG.ElemTypes as E (
    ElemM'
  , PortName
  , runElemM
  , evNewInt
  , evNewResource
  , evSetIdent
  , evSetClass
  , evSetType
  , evUID
  , evClass
  , evType
  , evResourceUsed
  , evPortVal
  , evNewPort
  , evNewResCons
  )
import qualified EDG.Elements.Port as E (
    extractPort
  , embedPort
  , assertPortUsed
  , areBarePortsConnected
  )
import qualified EDG.Elements.Elem as E (
    extractLink
  , extractModule
  , embedLink
  , embedModule
  , assertModuleUsed
  , assertLinkUsed
  , createOptionalConnection
  , createAllOptionalConnections
  , finishUpConstraints
  )
import qualified EDG.AssembleGraph as E (
    DecodeBlock(dbGraph)
  , DecodeGraph
  , decodeResult
  )
import qualified EDG.Elements as E (
    addModule
  , addLink
  )
import qualified EDG.Library.Types as E (
    Value(..)
  , Value'
  , Constrained(..)
  , Constrained'
  , UID'
  , Record
  , RecordCons(..)
  , IntCons
  , BoolCons
  , FloatCons
  , StringCons
  , UIDCons
  , RecCons
  , UIDNewtypeCons(UCBottom,UCNew)
  , Kinded(Int,Bool,Float,String,UID,Record,KVBot)
  , (<:=)
  , (<~=)
  )
import qualified EDG.Graphviz as E (
    genGraphOld
  , genGraphVerbose
  , genGraphSimple
  , writeGraph
  )
-- * Values and Constraints

-- | Integer value type, define using <:=
pattern IntV :: Integer -> AmbigVal
pattern IntV a = A.Concrete(E.Value (E.Int a))

-- | Integer constraint type, still define using <:=
pattern IntC :: E.IntCons -> AmbigVal
pattern IntC a = A.Abstract(E.Constrained (E.Int a))

-- | TODO
pattern BoolV :: Bool -> AmbigVal
pattern BoolV a = A.Concrete(E.Value (E.Bool a))

-- | TODO
pattern BoolC :: E.BoolCons -> AmbigVal
pattern BoolC a = A.Abstract(E.Constrained (E.Bool a))

-- | TODO
pattern FloatV :: Float -> AmbigVal
pattern FloatV a = A.Concrete(E.Value (E.Float a))

-- | TODO
pattern FloatC :: E.FloatCons -> AmbigVal
pattern FloatC a = A.Abstract(E.Constrained (E.Float a))

-- | TODO
pattern StringV :: String -> AmbigVal
pattern StringV a = A.Concrete(E.Value (E.String a))

-- | TODO
pattern StringC :: E.StringCons -> AmbigVal
pattern StringC a = A.Abstract(E.Constrained (E.String a))

-- | TODO
pattern UID :: AmbigVal
pattern UID = A.Abstract(E.Constrained (E.UID (E.UCBottom)))

-- | Allocate a new unique UID
pattern NewUID :: AmbigVal
pattern NewUID = A.Abstract(E.Constrained (E.UID (E.UCNew)))

-- | TODO
pattern Record :: E.RecCons -> AmbigVal
pattern Record a = A.Abstract(E.Constrained (E.Record a))

-- | Creates a range. Arguments are min then max.
--
-- >>> range (FloatC [greaterThan 4]) (FloatV 12)
range :: AmbigVal -> AmbigVal -> AmbigVal
range min max = Record ["min" <:= min, "max" <:= max]

-- | TODO
validRange :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> Exp m
validRange v = (GetField v "max") :>= (GetField v "min")

-- | Initialize a range, adding neccesary constraints.
initRange :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> m ()
initRange = constrain . validRange

-- | TODO
rContained :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> Exp m -> Exp m
rContained a r
  =   (rMin :<= rMax)
  :&& (a :>= rMin) :&& (a :<= rMax)
  where
    rMin = GetField r "min"
    rMax = GetField r "max"

rSubset :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> Exp m -> Exp m
rSubset a b
  =   (aMin :<= aMax)
  :&& (bMin :<= aMin) :&& (aMin :<= bMax)
  :&& (bMin :<= aMax) :&& (aMax :<= bMax)
  where
    aMin = GetField a "min"
    aMax = GetField a "max"
    bMin = GetField b "min"
    bMax = GetField b "max"

-- | TODO
rSuperset :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> Exp m -> Exp m
rSuperset = flip rSubset

rNotDisjoint :: (Monad m, Constrainable m, Exp m ~ E.Exp (PExp m))
          => Exp m -> Exp m -> Exp m
rNotDisjoint a b
  =   (aMin :<= aMax) :&& (bMin :<= bMax)
  :&& (bMin :<= aMax) :&& (aMin :<= bMax)
  where
    aMin = GetField a "min"
    aMax = GetField a "max"
    bMin = GetField b "min"
    bMax = GetField b "max"

-- ranges
--   - subset
--   - superset
--   - for constrants and pairs of ranges
--      - add
--      - subtract
--      - multiply
--   - bootstrap range etc.

-- | TODO
unknown :: A.BoundedJoinSemiLattice a => a
unknown = A.bottom

-- | TODO
oneOf :: E.OneOfConstraint a => [A.PredDom a] -> a
oneOf = E.oneOf

-- | TODO
noneOf :: E.NoneOfConstraint a => [A.PredDom a] -> a
noneOf = E.noneOf

-- | TODO
greaterThan :: E.GTConstraint a => A.PredDom a -> a
greaterThan = E.greaterThan

-- | TODO
greaterThanEq :: E.GTConstraint a => A.PredDom a -> a
greaterThanEq = E.greaterThanEq

-- | TODO
lessThan :: E.LTConstraint a => A.PredDom a -> a
lessThan = E.lessThan

-- | TODO
lessThanEq :: E.LTConstraint a => A.PredDom a -> a
lessThanEq = E.lessThanEq

-- | TODO
(+/-) :: forall a. (E.LTConstraint a, E.GTConstraint a
          , Num (A.PredDom a), GHC.Exts.IsList a, GHC.Exts.Item a ~ a)
          => A.PredDom a -> A.PredDom a -> a
a +/- b = [(greaterThanEq $ a - b :: a),(lessThanEq $ a + b :: a)]

-- | TODO
between :: forall a. (E.LTConstraint a, E.GTConstraint a
          , Num (A.PredDom a), Ord (A.PredDom a)
          ,GHC.Exts.IsList a, GHC.Exts.Item a ~ a)
          => A.PredDom a -> A.PredDom a -> a
between a b = [greaterThanEq (min a b),lessThanEq (max a b)]



-- | TODO :: The type of a portion of a record.
type AmbigRec = E.RecCons

-- | TODO
type AmbigVal = A.Ambiguous E.Value

infixr 0 <:=

-- | TODO
(<:=) :: String -> AmbigVal -> (String, AmbigVal)
(<:=) = (,)

-- test1 :: (String,AmbigVal)
-- test1 = "foo" <:= RecordV [
--     "a" <:= IntV 4
--   , "b" <:= IntC $ oneOf [3,4,5]
--   ]

-- * Expressions

-- | TODO
pattern Lit :: (Constrainable a, Exp a ~ E.Exp b
   , E.ExpContext b, E.ExpLiteral b ~ AmbigVal) => AmbigVal -> Exp a
pattern Lit a = E.Lit a

-- | TODO
pattern Val a = E.Val a

infix 4 :==
-- | TODO
pattern a :== b = a E.:== b

infix 4 :/=
-- | TODO
pattern a :/= b = a E.:/= b


infixr 3 :&&
-- | TODO
pattern a :&& b = a E.:&& b
infixr 2 :||
-- | TODO
pattern a :|| b = a E.:|| b
-- | TODO
pattern a :~& b = a E.:~& b
-- | TODO
pattern a :~| b = a E.:~| b
infixl 8 :<+>
-- | TODO
pattern a :<+> b = a E.:<+> b

infixl 1 :=>
-- | TODO
pattern a :=> b = a E.:=> b
-- | TODO
pattern Not a = E.Not a
-- | TODO
pattern JustOne a = E.JustOne a
-- | TODO
pattern All a = E.All a
-- | TODO
pattern Any a = E.Any a

infix 4 :<
-- | TODO
pattern a :< b = a E.:< b
infix 4 :<=
-- | TODO
pattern a :<= b = a E.:<= b
infix 4 :>
-- | TODO
pattern a :> b = a E.:>b
infix 4 :>=
-- | TODO
pattern a :>= b = a E.:>= b

infixl 6 :+
-- | TODO
pattern a :+ b = a E.:+ b
infixl 6 :-
-- | TODO
pattern a :- b = a E.:- b
infixl 7 :*
-- | TODO
pattern a :* b = a E.:* b
-- | TODO
pattern Sum a = E.Sum a
-- | TODO
pattern Mult a = E.Mult a
-- | TODO
pattern Negate a = E.Negate a

-- | TODO
pattern If c t f = E.If c t f
-- | TODO
pattern Count a = E.Count a
-- | TODO
pattern GetField r s = E.GetField s r

-- * Elements of a Design

-- | TODO :: Further Documentation
type Module = E.ElemM' E.Module E.ModPort

-- | TODO :: Further Documentation
type Link = E.ElemM' E.Link E.LinkPort

-- | TODO :: Further Documentation
type ModulePort = E.PortM E.ModPort

-- | TODO :: Further Documentation
type LinkPort = E.PortM E.LinkPort

-- | TODO :: Further Documentation
type PortName = E.PortName

-- | TODO
type ResourceUse m = (String,[Resource m])

-- | TODO
pattern (:|=) :: IsBlock m => String -> [Resource m] -> (String, [Resource m])
pattern a :|= b = (a,b)

-- | TODO :: Further Documentation
class (Monad m
  , Exp m ~ E.Exp (PExp m)
  , E.ExpContext (PExp m)
  , E.ExpLiteral (PExp m) ~ AmbigVal)
  => Constrainable m where

  -- | TODO :: Further Documentation
  type Exp m = f | f -> m

  type PExp m = b | b -> m

  -- | TODO :: Further Documentation
  constrain :: Exp m -> m ()

instance Constrainable Module where
  type Exp Module = E.Exp E.Module
  type PExp Module = E.Module
  constrain :: Exp Module -> Module ()
  constrain = S.constrain

instance Constrainable Link where
  type Exp Link = E.Exp E.Link
  type PExp Link = E.Link
  constrain :: Exp Link -> Link ()
  constrain = S.constrain

instance Constrainable ModulePort where
  type Exp ModulePort = E.Exp E.ModPort
  type PExp ModulePort = E.ModPort
  constrain :: Exp ModulePort -> ModulePort ()
  constrain = S.constrain

instance Constrainable LinkPort where
  type Exp LinkPort = E.Exp E.LinkPort
  type PExp LinkPort = E.LinkPort
  constrain :: Exp LinkPort -> LinkPort ()
  constrain = S.constrain

-- | TODO :: Further Documentation
class (Constrainable m) => IsElem m where

  -- | TODO
  setIdent :: String -> m ()

  -- | Set the type of the element to an ambiguous record.
  setType :: AmbigRec -> m (Exp m)

  -- | TODO
  uid :: Exp m

  -- | TODO
  typeVal :: String -> Exp m

  -- | TODO
  uniqueName :: String -> m String

-- | TODO
appendIdent :: IsElem m => String -> m ()
appendIdent = setIdent

-- | Identical to `setType`.
--   It is designed to be used in situations where you expect a type to
--   already be set, and are merely adding or firther constraining fields of
--   that type.
--
--   *This does **not** check that a type was already set beforehand. It will
--   act just like `setType` even if it is called first.*
--
--   TODO :: Examples.
updateType :: IsElem m => AmbigRec -> m (Exp m)
updateType = setType

makeName :: String -> Integer -> String
makeName s i = s ++ "[!" ++ show i ++ "]"

instance IsElem Module where
  setIdent :: String -> Module ()
  setIdent = E.evSetIdent
  setType :: AmbigRec -> Module (Exp Module)
  setType = E.evSetType
  uid :: Exp Module
  uid = E.evUID
  typeVal :: String -> Exp Module
  typeVal = E.evType
  uniqueName :: String -> Module String
  uniqueName s = makeName s <$> E.evNewInt

instance IsElem Link where
  setIdent :: String -> Link ()
  setIdent = E.evSetIdent
  setType :: AmbigRec -> Link (Exp Link)
  setType = E.evSetType
  uid :: Exp Link
  uid = E.evUID
  typeVal :: String -> Exp Link
  typeVal = E.evType
  uniqueName :: String -> Link String
  uniqueName s = makeName s <$> E.evNewInt

instance IsElem ModulePort where
  setIdent :: String -> ModulePort ()
  setIdent = E.pvSetIdent
  setType :: AmbigRec -> ModulePort (Exp ModulePort)
  setType = E.pvSetType
  uid :: Exp ModulePort
  uid = E.pvUID
  typeVal :: String -> Exp ModulePort
  typeVal = E.pvType
  uniqueName :: String -> ModulePort String
  uniqueName s = makeName s <$> E.pvNewInt

instance IsElem LinkPort where
  setIdent :: String -> LinkPort ()
  setIdent = E.pvSetIdent
  setType :: AmbigRec -> LinkPort (Exp LinkPort)
  setType = E.pvSetType
  uid :: Exp LinkPort
  uid = E.pvUID
  typeVal :: String -> Exp LinkPort
  typeVal = E.pvType
  uniqueName :: String -> LinkPort String
  uniqueName s = makeName s <$> E.pvNewInt

-- | TODO :: Further Documentation
class (IsElem p) => IsPort p where

  -- Ports can only be attached to ports of the same Kind
  -- (this saves effort for the SAT solver)
  setKind :: String -> p (Exp p)

  -- | TODO
  kind :: Exp p

  -- | TODO
  connected :: Exp p

-- | Identical to `setClass`.
--   It's designed to make it more clear that the class of the port is being
--   completely replaced by this operation, instead of simply changing it or
--   appending to it.
--
--   *This does **not** check that a class has already been set, and will act
--   just like `setClass` if it is called first, in particular future calls of
--   `setClass` or `replaceClass` will override it*
--
--   TODO :: Examples
replaceKind :: IsPort p => String -> p (Exp p)
replaceKind = setKind

instance IsPort ModulePort where
  setKind :: String -> ModulePort (Exp ModulePort)
  setKind = E.pvSetClass
  kind :: Exp ModulePort
  kind = E.pvClass
  connected :: Exp ModulePort
  connected = E.pvConnected

instance IsPort LinkPort where
  setKind :: String -> LinkPort (Exp LinkPort)
  setKind = E.pvSetClass
  kind :: Exp LinkPort
  kind = E.pvClass
  connected :: Exp LinkPort
  connected = E.pvConnected

-- | TODO :: Further Documentation
class (IsElem m, IsPort (PortType m)
  ,Resource m ~ E.Resource (PExp m)) => IsBlock m where

  -- | TODO
  type Resource m :: *

  type PortType m :: * -> *

  -- addPort: first call defines the port,
  -- further calls successively refine the type
  -- addPort returns an identifier, which may or may no be useful
  -- can refer to ports by either string name or identifier
  -- This happens in the module context,
  addPort :: String -> PortType m () -> m PortName

  -- | TODO :: Further Documentation
  setSignature :: String -> m (Exp m)

  -- Create resources (single-use things the system can use)
  -- i.e. pins, timer registers, DMA - unique entities which have to be
  -- conserved
  -- In synthesis process, all you care about is their name, it has no actual
  -- understanding
  newResource :: String -> m (Resource m)

  -- | TODO :: Further Documentation
  constrainResources :: String -> Exp m -> [ResourceUse m] -> m ()

  -- | TODO
  signature :: Exp m

  -- | TODO
  resourceUsed :: Resource m -> Exp m

  -- | TODO
  port :: String -> Exp (PortType m) -> Exp m

-- | TODO :: Further Documentation
replaceSignature :: IsBlock m => String -> m (Exp m)
replaceSignature = setSignature

instance IsBlock Module where
  type Resource Module = E.Resource E.Module
  type PortType Module = ModulePort
  addPort :: String -> ModulePort () -> Module PortName
  addPort = E.evNewPort
  setSignature :: String -> Module (Exp Module)
  setSignature = E.evSetClass
  newResource :: String -> Module (Resource Module)
  newResource = E.evNewResource
  constrainResources :: String -> Exp Module -> [ResourceUse Module]
                     -> Module ()
  constrainResources s e ml = E.evNewResCons s e
    (Map.map Set.fromList . Map.fromList $ ml)
  signature :: Exp Module
  signature = E.evClass
  resourceUsed :: Resource Module -> Exp Module
  resourceUsed = E.evResourceUsed
  port :: String -> Exp ModulePort -> Exp Module
  port s (Val d) = E.evPortVal s d
  port s e = (error $ "One cannot refer to port `" ++ s
    ++ "` with a resource of type `" ++ show e ++ "`" :: Exp Module)

instance IsBlock Link where
  type Resource Link = E.Resource E.Link
  type PortType Link = LinkPort
  addPort :: String -> LinkPort () -> Link PortName
  addPort = E.evNewPort
  setSignature :: String -> Link (Exp Link)
  setSignature = E.evSetClass
  newResource :: String -> Link (Resource Link)
  newResource = E.evNewResource
  constrainResources :: String -> Exp Link -> [ResourceUse Link]
                     -> Link ()
  constrainResources s e ml = E.evNewResCons s e
    (Map.map Set.fromList . Map.fromList $ ml)
  signature :: Exp Link
  signature = E.evClass
  resourceUsed :: Resource Link -> Exp Link
  resourceUsed = E.evResourceUsed
  port :: String -> Exp LinkPort -> Exp Link
  port s (Val d) = E.evPortVal s d
  port s e = (error $ "One cannot refer to port `" ++ s
    ++ "` with a resource of type `" ++ show e ++ "`" :: Exp Link)

-- * Building a Device Generation Problem

-- | TODO
data EDGSettings = EDGSettings {
    verboseSBV :: Bool
  , printOutput :: Bool
  , outputFile :: Maybe FilePath
  , graphvizFiles :: [(String,FilePath)]
  , smtLibFile :: Maybe FilePath
  , supressSMT :: Bool
  }

-- | TODO
defaultSettings :: EDGSettings
defaultSettings = EDGSettings{
    verboseSBV = False
  , printOutput = False
  , outputFile = Nothing
  , graphvizFiles = []
  , smtLibFile = Nothing
  , supressSMT = False
  }

parseSettings :: Parser EDGSettings
parseSettings = EDGSettings
  <$> (switch
          $  long "verboseSMT"
          <> short 'V'
          <> help "Print the full input problem sent to the SMT solver"
          <> showDefault
      )
  <*> (switch
        $  long "stdout"
        <> short 't'
        <> help "Print the output to STDOUT"
        <> showDefault
      )
  <*> (optional . strOption
        $  long "output"
        <> short 'o'
        <> metavar "<FILE>"
        <> help "Write the output to FILE"
      )
  <*> (many . fmap parseGraphvizOption . strOption
        $  long "graph-output"
        <> short 'g'
        <> metavar "<FILE or TYPE=FILE>"
        <> help ("Write the graph to FILE. Many supported filetypes "
          ++ "incl. 'png','svg','dot','pdf','gif','bmp',etc.."
          ++ "\nThis option can be used multiple times to create multiple "
          ++ "files."
          ++ "\n\nIf you use the TYPE=FILE syntax, as in `-g verbose=foo.png`"
          ++ " you can choose the rendering style of the output graph."
          ++ "\nOptions for TYPE are"
          ++ "\n 1) SIMPLE, the default mode that elides link and port"
          ++ " information."
          ++ "\n 2) VERBOSE, the layout mode that keeps all the link and port "
          ++ "information intact, but produces much larger graphs."
          ++ "\n 3) OLD, the old clustered output mode, which shows ports in"
          ++ " both the link and module."
          ++ "\n 4) DEFAULT, Option chosen if no type is specified, currently"
          ++ " 'simple'.")
      )
  <*> (optional . strOption
        $  long "smt-lib-output"
        <> short 's'
        <> metavar "<FILE>"
        <> help ("Write the raw SMT-LIB output to FILE. Mainy useful for "
          ++ "debugging and seeing how large things are.")
      )
  <*> (switch
        $  long "skip-smt"
        <> help ("Skip the SMT solving phase of the process, useful for "
          ++ "profiling.")
      )

parseGraphvizOption :: String -> (String,FilePath)
parseGraphvizOption s
  | (x:y:[]) <- splitString = (x,y)
  | not $ elem '=' s = ("default",s)
  | otherwise = error $ "Cannot use filenames with an '=' symbol."
    ++ show splitString
  where
    splitString = split '=' s

-- | TODO
data EDGLibrary = EDGLibrary {
    modules :: [(String,Int,Module ())]
  , links :: [(String,Int,Link ())]
  }

-- * Synthesizing a Device

-- | TODO
synthesize :: EDGLibrary -> String -> Module () -> IO ()
synthesize l n s = synthesizeWithSettings defaultSettings l [(n,s)]

-- | TODO
makeSynthFunc :: EDGLibrary -> [(String,Module ())]
              -> EDGSettings -> IO ()
makeSynthFunc l m s = synthesizeWithSettings s l m

deriving instance Generic SBV.SatResult
deriving instance NFData SBV.SatResult

-- | Wrapper type for a model that should keep us from having to
--   constantly recalculate the dictionary
data ModelableWrapper a = MW{
    model :: a
  , dict :: Map String SBV.CW
  , modVal :: forall b. SBV.SymWord b => String -> Maybe b
  }

instance SBV.Modelable a => SBV.Modelable (ModelableWrapper a) where
  modelExists = SBV.modelExists . model
  getModel = SBV.getModel . model
  getModelDictionary = dict
  getModelValue s a = modVal a s
  getModelUninterpretedValue s = SBV.getModelUninterpretedValue s . model
  extractModel = SBV.extractModel . model

-- | Wrap a modelable to cache the dictionary
wrapModel :: SBV.Modelable a => a -> ModelableWrapper a
wrapModel a = MW{
    model = a
  , dict = dc
  , modVal = (\ s -> SBV.fromCW <$> Map.lookup s dc)
  }
  where
    dc = SBV.getModelDictionary a


-- | TODO
synthesizeWithSettings :: EDGSettings
                       -> EDGLibrary -> [(String,Module ())] -> IO ()
synthesizeWithSettings EDGSettings{..} EDGLibrary{..} seeds =
  time "Design Synthesis" $ do
    ss <- IO.newIORef (undefined :: E.SBVState)
    -- Solve the initial sat problem
    (symbM,gatherState,sm) <- time "Precomputation" $
      evaluate . (\ (a,b,c) -> (a,force b,c)) $ E.runEDGMonad (Just ss) edgm
    when supressSMT exitSuccess
    solution :: SBV.SatResult <- time "Sat Solving" $ (evaluate . force) =<<
      (SBV.satWith SBV.defaultSMTCfg{SBV.verbose = verboseSBV} symbM)
    case solution of
      SBV.SatResult (SBV.Satisfiable _ _) -> do
        (decodeState,decodeResult',sbvState) <- time "Decoding SAT Output" $ do
            sbvState <- IO.readIORef ss
            decodeState <- evaluate . force $
              E.buildDecodeState gatherState sbvState
            decodeResult' <- evaluate . force $
              E.decodeResult decodeState (wrapModel solution) sm
            return (decodeState, decodeResult',sbvState)
        case decodeResult' of
          Left s -> do
            putStrLn $ "Resulting solution was : "
            E.pPrint $ solution
            putStrLn $ "\n\n"
            putStrLn $ "Decode of solution failed with : " ++ s
            return ()
          Right decodeResult -> time "Building Output Files" $ do
            -- Print output
            when printOutput $ E.pPrint decodeResult
            -- Generate and print smtlib file
            sequence_ $ time "generating/writing smt-lib file"
              <$> (flip fmap smtLibFile $ \ f -> do
                s <- SBV.compileToSMTLib SBV.SMTLib2 True symbM
                writeFile f s)
            -- TODO :: Write output to file
            time "writing output data file" $ sequence_ $
              flip T.writeFile (T.pShowNoColor decodeResult) <$> outputFile
            time "Writing Graph" $ do
              let outputGraphSimple  = E.genGraphSimple decodeResult
                  outputGraphOld     = E.genGraphOld decodeResult
                  outputGraphVerbose = E.genGraphVerbose decodeResult
                  outputGraphDefault = outputGraphSimple
              flip mapM_ graphvizFiles $ \ (graphType, filename) -> do
                case map toLower graphType of
                  "simple" -> E.writeGraph outputGraphSimple filename
                  "old" -> E.writeGraph outputGraphOld filename
                  "verbose" -> E.writeGraph outputGraphVerbose filename
                  "default" -> E.writeGraph outputGraphDefault filename
                  _ -> error $ "Invalid TYPE, options are : OLD,"
                    ++ " VERBOSE, and SIMPLE."
              return ()
      _ -> do
        E.pPrint solution
        return ()

  where
    edgm = do
      let ls = concat . map makeDups $ links
          ms = concat . map makeDups $ modules
      trace "adding links" $ mapM_ (uncurry E.addLink) ls
      trace "adding modules" $ mapM_ (uncurry E.addModule) ms
      seedRefs <- trace "adding seeds " $
        flip mapM seeds $ \(seedName,seedModule) -> do
          seed <- E.addModule seedName seedModule
          E.assertModuleUsed seed
          return seed
      -- NOTE :: Any changes must happen before this point otherwise
      --         you'll break the optional constraints thing.
      trace "adding connections" $ E.createAllOptionalConnections
      trace "cleaning up first pass" $ E.finishUpConstraints
      return (head seedRefs)

    makeDups :: (String,Int,b) -> [(String,b)]
    makeDups (name, count, b)
      = map (\ (num,m) -> (name ++ "[" ++ show num ++ "]",m))
        . zip [1..] . replicate count $ b

    time :: String -> IO t -> IO t
    time s a = do
        putStrLn $ "Starting : " ++ s
        cStart <- getCPUTime
        wStart <- getCurrentTime
        v <- a
        cEnd   <- getCPUTime
        wEnd <- getCurrentTime
        putStrLn $ "Finished : " ++ s
        let cDiff = (fromIntegral (cEnd - cStart)) / (10^12)
            wDiff = (fromRational . toRational $ diffUTCTime wEnd wStart)-- / (10^12)
        printf ("Computation time (%s):\n"
          ++ "  cpu  : %0.3f sec\n"
          ++ "  wall : %0.3f sec\n" :: String)
          (s :: String)(cDiff :: Double)(wDiff :: Double)
        return v
-- * Utility Functions

-- | TODO
endDef :: Monad m => m ()
endDef = return ()
