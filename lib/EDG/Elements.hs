
-- | This module is where we'll bundle up all of the functions, contructors,
--   and similar important fiddlies that are needed for the actual library
--   development and testing work.
--
-- TODO :: Rename this to something better, like "EDG" so that it's the
--         obvious minimal import to start developing an actual tool.
-- TODO :: Export the set of minimal imports and then make sure you move the
--         test problem out of this section.
module EDG.Elements where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Control.Lens.Ether.Implicit
import Data.Maybe (fromJust)

import Algebra.Lattice (
    bottom
  , BoundedJoinSemiLattice
  )
import Algebra.AsPredicate (
    LiftablePredicate
  , liftPredicate
  )
import Algebra.Constrainable (
    Ambiguous(..)
  )
import EDG.Predicates (
    oneOf
  , noneOf
  , lessThan
  , lessThanEq
  , greaterThan
  , greaterThanEq
  )
import Control.Monad.MonadSymbolic (
    constrain
  , Symbolic
  , SBV
  )
import EDG.Library.Types (
    Value(..)
  , Kinded(Int,Bool,String,UID,Record,KVBot)
  , (<:=)
  , (<~=)
  )
import EDG.Library.Types.TypeVal (
  )
import EDG.Expression (
    Exp(..)
  )
import EDG.EDGInstances (runEDGMonad)
import EDG.EDGInstances.Bool (
    preventConjunction
  )
import EDG.EDGDatatype (
    Ref(..)
  , Port
  , EDG
  , Link
  , Module
  , ModPort
  , LinkPort
  )
import EDG.EDGMonad (
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
  , SMTResult
  )
import Data.IORef (IORef,newIORef,readIORef)
import EDG.ElemIncludes (pPrint)
import EDG.PortTypes (
    convertPortState
  , PortM
  , PortDesc
  , PortValue(..)
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
import EDG.ElemTypes (
    ElemM
  , runElemM
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
import EDG.Elements.Port (
    extractPort
  , embedPort
  , assertPortUsed
  , areBarePortsConnected
  )
import EDG.Elements.Elem (
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
import EDG.AssembleGraph (
    DecodeBlock(dbGraph)
  , DecodeGraph
  , decodeResult
  )

addModule :: String -> ElemM Module () -> EDGMonad (Ref Module)
addModule s m = embedModule s $ runElemM m

addLink :: String -> ElemM Link () -> EDGMonad (Ref Link)
addLink s m = embedLink s $ runElemM m

