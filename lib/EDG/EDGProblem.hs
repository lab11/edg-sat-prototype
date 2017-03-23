

-- | Random catchall module for test cases and the functions that unroll the
--   monads.
--
--   TODO :: FIND A BETTER PLACE TO KEEP ALL THIS SHIT.
--
module EDG.EDGProblem where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Text.Pretty.Simple as P

import Control.Newtype

import Control.Monad.Ether.Implicit
import Control.Monad.MonadSymbolic
import Data.SBV (
  Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  )
import qualified Data.SBV as S

import Data.IORef

import Control.Monad.Scribe
import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class

import Control.Lens.Ether.Implicit
import Control.Lens.TH

import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Library.Types
import EDG.Predicates

import EDG.EDGMonad
import EDG.EDGDatatype

import EDG.EDGInstances

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = P.pPrintOpt P.defaultOutputOptionsDarkBg{P.outputOptionsIndentAmount=2}

-- | Just a test problem I'll be editing a lot.
--
--   TODO ;: Change this interface so it's easier to get the RefType values
--           back out once you're done. Not sure how that'll look either.
--
testProblem = do
  -- b1 <- refAbstract @Value "b1" (pack $ KVBot ())
  b1 <- refAbstract @Value "b1" (pack . Record $ [
      "field1" <~= Int $ oneOf [1,8,15]
    , "field2" <~= String $ bottom
    , "field3" <~= Float $ [lessThan 12, greaterThan 3]
    , "field4" <~= Record $ bottom
    , "field5" <~= KVBot ()
    ])
  --b2 <- refAbstract @Value "b2" (pack . Int $ oneOf[3,4,5,6])
  b2 <- refAbstract @Value "b2" (pack . Record $ [
      "field1" <~= Int $ [lessThan 12, greaterThan 5]
    , "field2" <~= String $ oneOf ["a","b","c"]
    , "field3" <~= KVBot ()
    , "field4" <~= Record $ [
        "field1" <:= Int $ 7
      , "field2" <:= String "b"
      ]
    , "field5" <~= KVBot ()
    ])
  b3 <- refAbstract @Value "b3" (pack . Record $ [
      "field1" <~= Int $ [lessThan 12, greaterThan 5]
    , "field2" <~= String $ oneOf ["a","b","c"]
    , "field3" <~= KVBot ()
    , "field4" <~= Record $ [
        "field1" <:= Int $ 4
      --, "field4" <~= KVBot ()
      , "field5" <~= String $ oneOf ["a","b"]
      ]
    , "field5" <~= KVBot ()
    ])
  b4 <- b1 .== b2
  b5 <- b1 .== b3
  b6 <- b4 .|| b5
  constrain b6
  b7 <- getValS b3 "field4.field5"
  b8 <- getVal "b1.field5"
  b9 <- getValL b2 ["field4","field2"]
  b10 <- b9 ./= b8
  b11 <- b7 .== b8
  constrain b10
  constrain b11
  -- b12 <- getVal "b2.field4"
  -- b13 <- getVal "b3.field4.field4"
  -- b14 <- b12 .== b13
  -- constrain b14
  return (b1,b2,b3)

-- | What `main` in "app/Main.hs" calls.
runTestProblem :: IO ()
runTestProblem = do
  let i = pack $ Int $ [lessThan 12, greaterThan 5]
  print @(Constrained) i
  print (unSAT (Abstract i))
  ss <- newIORef (undefined :: SBVState)
  let (symb,gs,(b1,b2,b3)) = runEDGMonad (Just ss) testProblem
  sol <- S.satWith S.defaultSMTCfg{S.verbose = True} symb
  pPrint sol
  ss' <- readIORef ss
  let decSt = buildDecodeState gs ss'
  putStrLn "RI:"
  pPrint (ss' ^. recInfo)
  putStrLn "RK:"
  pPrint (ss' ^. recordKinds)
  putStrLn "B1:"
  pPrint $ extract decSt sol b1
  putStrLn "B2:"
  pPrint $ extract decSt sol b2
  putStrLn "B3:"
  pPrint $ extract decSt sol b3



-- TODO :: Figure out why we're not getting an error during the looping
--         type resolution for the following input, That this produces a
--         completely different error for something completely different is
--         kinda absurd.
--
-- testProblem = do
--   -- b1 <- refAbstract @Value "b1" (pack $ KVBot ())
--   b1 <- refAbstract @Value "b1" (pack . Record $ [
--       "field1" <~= Int $ oneOf [1,8,15]
--     , "field2" <~= String $ bottom
--     , "field3" <~= Float $ [lessThan 12, greaterThan 3]
--     , "field4" <~= Record $ bottom
--     , "field5" <~= KVBot ()
--     ])
--   --b2 <- refAbstract @Value "b2" (pack . Int $ oneOf[3,4,5,6])
--   b2 <- refAbstract @Value "b2" (pack . Record $ [
--       "field1" <~= Int $ [lessThan 12, greaterThan 5]
--     , "field2" <~= String $ oneOf ["a","b","c"]
--     , "field3" <~= KVBot ()
--     , "field4" <~= Record $ [
--         "field1" <:= Int $ 7
--       , "field2" <:= String "b"
--       ]
--     , "field5" <~= KVBot ()
--     ])
--   b3 <- refAbstract @Value "b3" (pack . Record $ [
--       "field1" <~= Int $ [lessThan 12, greaterThan 5]
--     , "field2" <~= String $ oneOf ["a","b","c"]
--     , "field3" <~= KVBot ()
--     , "field4" <~= Record $ [
--         "field1" <:= Int $ 4
--       , "field4" <~= KVBot ()
--       , "field5" <~= String $ oneOf ["a","b"]
--       ]
--     , "field5" <~= KVBot ()
--     ])
--   b4 <- b1 .== b2
--   b5 <- b1 .== b3
--   b6 <- b4 .|| b5
--   constrain b6
--   b7 <- getValS b3 "field4.field5"
--   b8 <- getVal "b1.field5"
--   b9 <- getValL b2 ["field4","field2"]
--   b10 <- b9 ./= b8
--   b11 <- b7 .== b8
--   constrain b10
--   constrain b11
--   b12 <- getVal "b2.field4"
--   b13 <- getVal "b3.field4.field4"
--   b14 <- b12 .== b13
--   constrain b14
--   return (b1,b2,b3)
