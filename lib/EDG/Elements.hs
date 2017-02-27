
-- | This module is where we'll bundle up all of the functions, contructors,
--   and similar important fiddlies that are needed for the actual library
--   development and testing work.
--
-- TODO :: Rename this to something better, like "EDG" so that it's the
--         obvious minimal import to start developing an actual tool.
-- TODO :: Export the set of minimal imports and then make sure you move the
--         test problem out of this section.
module EDG.Elements where

import EDG.Library.Types (runEDGMonad)
import EDG.EDGInstances (Value(..))
import EDG.Datatype (Ref,Port)
import EDG.EDGMonad (
    SBVState
  , GatherState
  , DecodeState
  , buildDecodeState
  )
import qualified Data.SBV as SBV (
    satWith
  , defaultSMTConfig
  , SMTConfig(..)
  )
import Data.IORef (IORef,newIORef,readIORef)
import EDG.ElemIncludes (pPrint)
import EDG.PortTypes ()
import EDG.Elements.Port (extractPort)
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
  solution <- SBV.satWith SBV.defaultSMTCfg{SBV.verbose = True} symbolicMonad
  sbvState <- readIORef ss
  let decodeState = buildDecodeState gatherState sbvState
  flip mapM_ (zip pl [1..]) $ \ (num,portRef) -> do
    putStrLn "nextPort #" ++ show num ++ " :"
    pPrint $ extractPort decodeState solution portRef

-- Example Problem --

testProblem :: EDGMonad [Ref Port]
testProblem = do
  undefined
  return undefined -- [p1,p2,p3,p4]
