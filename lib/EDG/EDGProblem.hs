

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
import Data.Map (Map)
import qualified Data.Map as Map
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


-- | Run the Gather Monad to, get either an error, or a tuple with the output
--   and the corresponding return value.
runGather :: GatherMonad a -> Either String (a,GatherState)
runGather = runIdentity . runExceptT . flip runStateT initialGatherState

-- | Given a particular variable which should be true, a start state, and
--   the monad, get the underlying Symbolic monad with the relevant sBool
--   chosen. This is a predicate we can work with pretty straightforwardly.
--
--   TODO :: Fix this super cludgy way of getting the insternal state of the
--           SBVMonad back out. There's got to be something that
--           allows us to get the value back out more elegantly. That said
--           if you can't think of one, refactor this interface so it's
--           less awyward but still lets you get the
--           state back out.
--
runSBVMonad :: SBVState -> Maybe (IORef SBVState) -> SBVMonad a -> Symbolic (SBV Bool)
runSBVMonad state sio monad = fmap throwUp . runExceptT $ evalStateT nm state
  where
    throwUp (Left err) = error $ "Execution failed in SBV phase with error:\n" ++ err
    throwUp (Right  v) = v

    -- Yup :V I'm grabbing the state through an IORef, this is incredibly not
    -- robust against using allSat or repeated invocations. There has to be
    -- some special provision to allow that.
    --
    -- TODO :: Though It might just be that I have to make sure that the
    --         decoding can work with only the completed GatherState. At the
    --         moment we're fine. But this is a change that we should consider.
    nm = do
      monad
      state <- get
      sequence ((\ r -> liftIO $ writeIORef r state) <$> sio)
      return $ S.literal True

-- | Turn the entire EDG monad into a predicate while propagating errors up to
--   the standard error system.
--
runEDGMonad :: Maybe (IORef SBVState) ->  EDGMonad a -> (Symbolic (SBV Bool),GatherState,a)
runEDGMonad sio i = case runGather . runScribeT $ i of
  Left err -> error $ "Execution failed in Gather phase with error:\n" ++ err
  Right ((a,sbvm),gs) -> (runSBVMonad (transformState gs) sio sbvm,gs,a)

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
    ])
  --b2 <- refAbstract @Value "b2" (pack . Int $ oneOf[3,4,5,6])
  b2 <- refAbstract @Value "b2" (pack . Record $ [
      "field1" <~= Int $ [lessThan 12, greaterThan 5]
    , "field2" <~= String $ oneOf ["a","b","c"]
    , "field3" <~= KVBot ()
    , "field4" <~= Record $ [
        "field1" <:= Int $ 4
      , "field2" <~= String $ oneOf ["a","b"]
      ]
    ])
  b3 <- refAbstract @Value "b3" (pack . Record $ [
      "field1" <~= Int $ [lessThan 12, greaterThan 5]
    , "field2" <~= String $ oneOf ["a","b","c"]
    , "field3" <~= KVBot ()
    , "field4" <~= Record $ [
        "field1" <:= Int $ 4
      , "field5" <~= String $ oneOf ["a","b"]
      ]
    ])
  b4 <- b1 .== b2
  b5 <- b1 .== b3
  b6 <- b5 .|| b5
  constrain $ b6
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




