
-- | This module is where we'll bundle up all of the functions, contructors,
--   and similar important fiddlies that are needed for the actual library
--   development and testing work.
--
-- TODO :: Rename this to something better, like "EDG" so that it's the
--         obvious minimal import to start developing an actual tool.
-- TODO :: Export the set of minimal imports and then make sure you move the
--         test problem out of this section.
module EDG.Elements where


import Algebra.Lattice (
    bottom
  , BoundedJoinSemiLattice
  )
import EDG.Predicates (
    oneOf
  , noneOf
  )
import Control.Monad.MonadSymbolic (
    constrain
  )
import EDG.Library.Types (
    Value(..)
  , Kinded(Int,Bool,String,UID,Record,KVBot)
  , (<:=)
  , (<~=)
  )
import EDG.Expression (
    Exp(..)
  )
import EDG.EDGInstances (runEDGMonad)
import EDG.EDGDatatype (Ref(..),Port)
import EDG.EDGMonad (
    SBVState
  , GatherState
  , DecodeState
  , buildDecodeState
  , EDGMonad
  )
import qualified Data.SBV as SBV (
    satWith
  , defaultSMTCfg
  , SMTConfig(..)
  )
import Data.IORef (IORef,newIORef,readIORef)
import EDG.ElemIncludes (pPrint)
import EDG.PortTypes (
    convertPortState
  , PortM
  , PortDesc
  , setIdent
  , setClass
  , setType
  , eUID
  , eConnected
  , eClass
  , eConnectedTo
  , eType
  , addLiteral
  , runPortM
  )
import EDG.Elements.Port (
    extractPort
  , embedPort
  , assertPortUsed
  , areBarePortsConnected
  )
-- ABSOLUTE MINIMAL IMPORT SET --

-- Utility Functions / Syntax Sugar --

-- | Solve the problem and display all the information about the various
--   major components in the design (basically just barePorts, Modules, Links,
--    and their connectivity graph)
--
--  TODO :: Implement this
--
-- solveAndDisplayAll :: EDGMonad i -> IO ()

-- | Solves a device generation problem
--
-- TODO :: Add a nice way to include settings.
--
-- solveProblem :: EDGMonad i -> IO ()
--   verbose? :: bool
--   extractVals :: Modelable a => DecodeState -> a -> i -> IO ()
solveProblem :: EDGMonad [Ref Port] -> IO ()
solveProblem edgm = do
  ss <- newIORef (undefined :: SBVState)
  let (symbolicMonad,gatherState,pl) = runEDGMonad (Just ss) edgm
  solution <- SBV.satWith SBV.defaultSMTCfg{SBV.verbose = False} symbolicMonad
  sbvState <- readIORef ss
  let decodeState = buildDecodeState gatherState sbvState
  flip mapM_ pl $ \ portRef -> do
    putStrLn $ "Next Port :"
    pPrint $ extractPort decodeState solution portRef
  return ()

addBarePort :: String -> PortM Port () -> EDGMonad (Ref Port)
addBarePort s m = embedPort s $ runPortM m

unknown :: BoundedJoinSemiLattice a => a
unknown = bottom

pattern Unknown = KVBot ()

-- Example Problem --

testProblem :: EDGMonad [Ref Port]
testProblem = do
  p1 <- addBarePort "p1" $ do
    setIdent "testPort"
    setClass "p"
    setType [
        "f1" <:= Int 5
      , "f2" <~= Unknown
      ]
    return ()
    -- constrain $ (eType "f1") :== (eType "f2")

  p2 <- addBarePort "p2" $ do
    setIdent "testPort"
    setClass "p"
    setType [
        "f1" <~= Int unknown
      , "f2" <~= Int [oneOf [8,12,16]]
      ]
    return ()
    -- constrain $ (eType "f1") :== (eType "f2")

  p1p2 <- areBarePortsConnected p1 p2

  assertPortUsed p1

  constrain p1p2

  return [p1,p2] -- ,p2,p3,p4]
