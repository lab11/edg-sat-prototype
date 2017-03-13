
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
solveProblem :: EDGMonad ([Ref Port],[Ref Link],[Ref Module]) -> IO ()
solveProblem edgm = do
  ss <- newIORef (undefined :: SBVState)
  let (symbolicMonad,gatherState,(pl,ll,ml)) = runEDGMonad (Just ss) modEDGm
  solution <- SBV.satWith SBV.defaultSMTCfg{SBV.verbose = False} symbolicMonad
  sbvState <- readIORef ss
  let decodeState = buildDecodeState gatherState sbvState
  -- pPrint decodeState
  -- flip mapM_ pl $ \ portRef -> do
  --   putStrLn $ "Next Port :"
  --   pPrint $ extractPort decodeState solution portRef
  -- flip mapM_ ll $ \ linkRef -> do
  --   putStrLn $ "Next Link :"
  --   pPrint $ extractLink decodeState solution linkRef
  -- flip mapM_ ml $ \ moduleRef -> do
  --   putStrLn $ "Next Module :"
  --   pPrint $ extractModule decodeState solution moduleRef
  putStrLn $ "Decoded Result :"
  pPrint $ (decodeResult decodeState solution (head ml))
  return ()

  where
    modEDGm = do
      output <- edgm
      -- We can only correctly constrain the ports once we know all of the
      -- potential connections they may have.
      finishUpConstraints
      return output

-- | Feh it just fiddles with minor parameters. If we want an actual search
--   of the space I'm going to have to find a good way to recover and reset
--   all the connection information so that the conjunction of all the chosen
--   options is disallowed.
shittySolveAllProblems :: EDGMonad ([Ref Port],[Ref Link],[Ref Module])
                       -> IO ()
shittySolveAllProblems edgm = do
  ss <- newIORef (undefined :: SBVState)
  let (symbolicMonad,gatherState,(pl,ll,ml)) = runEDGMonad (Just ss) modEDGm
  solution <- SBV.allSatWith SBV.defaultSMTCfg{SBV.verbose = False} symbolicMonad
  sbvState <- readIORef ss
  let decodeState = buildDecodeState gatherState sbvState
  let SBV.AllSatResult (_,sols) = solution
  flip mapM_ sols $ \ sol -> do
    putStrLn $ "Decoded Result :"
    pPrint $ decodeResult decodeState sol (head ml)
  return ()

  where
    modEDGm = do
      output <- edgm
      -- We can only correctly constrain the ports once we know all of the
      -- potential connections they may have.
      finishUpConstraints
      return output

-- TODO :: When you feel up to it, use the gsConnectionVars variable to create
--         a better AllSAT. At each round, run the symbolic calculation, use
--         the solution to find the full list of connections that are used,
--         extend the symbolic monad with a thing that keeps their conjunction
--         from appearing, and loop until heat death of the universe.
--
-- solveAllProblems :: EDGMonad ([Ref Port],[Ref Link],[Ref Module]) -> IO ()
-- solveAllProblems edgm = do
--   ss <- newIORef (undefined :: SBVState)
--   let (symbolicMonad ,gatherState,(pl,ll,ml)) = runEDGMonad (Just ss) modEDGm
--   -- let SBV.AllSatResult (_,sols) = solution
--   -- flip mapM_ sols $ \ sol -> do
--   --   putStrLn $ "Decoded Result :"
--   --   pPrint $ decodeResult decodeState sol (head ml)
--   -- return ()
--   undefined
--   where
--     solveOnce :: IORef SBVState
--               -> GatherState
--               -> Ref Module
--               -> SBVMonad ()
--               -> IO (Either String DecodeBlock, SBVMonad ())
--     solveOnce ss gatherState seed symb = do
--       solution <- SBV.satWith SBV.defaultSMTCfg{SBV.verbose = False} symb
--       sbvState <- readIORef ss
--       let decodeState = buildDecodeState gatherState sbvState
--           decodeBlk = decodeResult decodeState solution seed
--           conns = decodeState ^. _1 . connectionVars
--           usedConns = Set.filter (fromJust . extract decodeState solution) conns
--       trace (show usedConns) $ return (decodeBlk,symb >> preventConjunction usedConns)
--
--     modEDGm = do
--       output <- edgm
--       -- We can only correctly constrain the ports once we know all of the
--       -- potential connections they may have.
--       finishUpConstraints
--       return output



addBarePort :: String -> PortM Port () -> EDGMonad (Ref Port)
addBarePort s m = embedPort s $ runPortM m

addModule :: String -> ElemM Module () -> EDGMonad (Ref Module)
addModule s m = embedModule s $ runElemM m

addLink :: String -> ElemM Link () -> EDGMonad (Ref Link)
addLink s m = embedLink s $ runElemM m

unknown :: BoundedJoinSemiLattice a => a
unknown = bottom

pattern Unknown = KVBot ()

testProblem :: EDGMonad ([Ref Port],[Ref Link],[Ref Module])
testProblem = do
  m1 <- addModule "seedModule" $ do
      evSetIdent "seedModule"
      evSetClass "seed"
      evSetType [
          "f1" <~= Int [greaterThan 5, lessThan 12]
        , "f2" <~= String $ oneOf ["test1","test2"]
        , "f3" <~= UID $ bottom
        , "f4" <~= Bool $ bottom
        , "f5" <~= Int [greaterThan 5, lessThan 12]
        , "f6" <~= Int bottom
        ]

      p1 <- evNewPort "port1" $ do
        pvSetIdent "port1"
        pvSetClass "a"
        pvSetType [
            "p3" <~= UID bottom
          ]
        return ()

      rt <- evNewResource "timer"
      rd <- evNewResource "dma"

      evNewResCons "useTimer"
        (evType "f4") $ Map.fromList [("e1",[rt,rd])]

      evNewResCons "useTimer2"
        (Not $ evType "f4") $ Map.fromList [("e2",[rt]),("e3",[rd])]


      constrain (Not $ evType "f4" :: Exp Module)
      constrain (evType "f3" :== evUID :: Exp Module)
      constrain (evType "f1" :/= evType "f5" :: Exp Module)
      constrain ((Negate $ evType "f1" :* evType "f5")
        :== evType "f6" :: Exp Module)
      constrain (evPortVal "port1" PVConnected :: Exp Module)

      return ()

  m2 <- addModule "otherModule" $ do
      evSetIdent "otherModule"
      evSetClass "other"
      evSetType [
          "f1" <~= Int [greaterThan 5, lessThan 12]
        , "f2" <~= String $ oneOf ["test1","test2"]
        , "f3" <~= UID $ bottom
        ]

      p1 <- evNewPort "port1" $ do
        pvSetIdent "port1"
        pvSetClass "a"
        pvSetType [
            "p3" <~= UID bottom
          ]
        return ()

      -- F3 is our UID
      constrain (evType "f3" :== evUID :: Exp Module)
      -- Out UID is equal to the UID in the port
      constrain (evType "f3"
        :== evPortVal "port1" (PVType ["p3"]) :: Exp Module)
      -- our port is connected
      constrain (evPortVal "port1" PVConnected :: Exp Module)

      -- constrain (mkLit $ Bool False :: Exp Module)
      return ()

  m3 <- addModule "otherModule" $ do
      evSetIdent "otherModule"
      evSetClass "other"
      evSetType [
          "f1" <~= Int [greaterThan 5, lessThan 12]
        , "f2" <~= String $ oneOf ["test1","test2"]
        , "f3" <~= UID $ bottom
        ]

      p1 <- evNewPort "port1" $ do
        pvSetIdent "port1"
        pvSetClass "a"
        pvSetType [
            "p3" <~= UID bottom
          ]
        return ()

      -- F3 is our UID
      constrain (evType "f3" :== evUID :: Exp Module)
      -- Out UID is equal to the UID in the port
      constrain (evType "f3"
        :== evPortVal "port1" (PVType ["p3"]) :: Exp Module)
      -- our port is connected
      constrain (evPortVal "port1" PVConnected :: Exp Module)
      -- constrain (mkLit $ Bool False :: Exp Module)
      return ()

  l1 <- addLink "testLink" $ do
    evSetIdent "testLink"
    evSetClass "aLink"

    p1 <- evNewPort "port1" $ do
      pvSetIdent "port1"
      pvSetClass "a"
      pvSetType [
          "p3" <~= UID bottom
        ]
      return ()

    p2 <- evNewPort "port2" $ do
      pvSetIdent "port2"
      pvSetClass "a"
      pvSetType [
          "p3" <~= UID bottom
        ]
      return ()

    constrain (evPortVal "port1" PVConnected
      :== evPortVal "port2" PVConnected :: Exp Link)


  l2 <- addLink "testLink" $ do
    evSetIdent "testLink"
    evSetClass "aLink"

    p1 <- evNewPort "port1" $ do
      pvSetIdent "port1"
      pvSetClass "a"
      pvSetType [
          "p3" <~= UID bottom
        ]
      return ()

    p2 <- evNewPort "port2" $ do
      pvSetIdent "port2"
      pvSetClass "a"
      pvSetType [
          "p3" <~= UID bottom
        ]
      return ()

    constrain (evPortVal "port1" PVConnected
      :== evPortVal "port2" PVConnected :: Exp Link)

  createAllOptionalConnections

  assertModuleUsed m1

  return ([],[l1,l2],[m1,m2,m3])

