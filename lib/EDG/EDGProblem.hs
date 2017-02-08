

-- | Random catchall module for test cases and the functions that unroll the
--   monads.
--
--   TODO :: FIND A BETTER PLACE TO KEEP ALL THIS SHIT.
--
module EDG.EDGProblem where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Newtype

import Control.Monad.Ether.Implicit
import Control.Monad.MonadSymbolic
import Data.SBV (
  Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
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

import EDG.EDGMonad
import EDG.EDGInstances


-- | Run the Gather Monad to, get either an error, or a tuple with the output
--   and the corresponding return value.
runGather :: GatherMonad a -> Either String (a,GatherState)
runGather = runIdentity . runExceptT . flip runStateT initialGatherState

-- | Given a particular variable which should be true, a start state, and
--   the monad, get the underlying Symbolic monad with the relevant sBool
--   chosen. This is a predicate we can work with pretty straightforwardly.
runSBVMonad :: Ref Bool -> SBVState -> SBVMonad a -> Symbolic (SBV Bool)
runSBVMonad target state monad = fmap throwUp . runExceptT $ evalStateT (monad >> sbv target) state
  where
    throwUp (Left err) = error $ "Execution failed in SBV phase with error: " ++ err
    throwUp (Right  v) = v

-- | Transform the ending state from the Gather pass into the start state for
--   the SBV pass.
transformState :: GatherState -> SBVState
transformState _ = SBVState {
    ssBoolRef = Map.empty
  }

-- | Turn the entire EDG monad into a predicate while propagating errors up to
--   the standard error system.
runEDGMonad :: EDGMonad (Ref Bool) -> Symbolic (SBV Bool)
runEDGMonad i = case runGather . runScribeT $ i of
  Left err -> error $ "Execution failed in Gather phase with error: " ++ err
  Right ((ref,sbvm),gs) -> runSBVMonad ref (transformState gs) sbvm

-- | Just a test problem I'll be editing a lot.
testProblem :: EDGMonad (Ref Bool)
testProblem = do
  b1 <- refAbstract @Bool "b1" bottom
  b2 <- refAbstract @Bool "b2" bottom
  b3 <- refAbstract @Bool "b3" bottom
  return b3

-- | What `main` in "app/Main.hs" calls.
runTestProblem :: IO ()
runTestProblem = do
  sol <- S.allSatWith S.defaultSMTCfg{S.verbose = False}  . runEDGMonad $ testProblem
  print sol

-- TODO :: Everything below this point is bullshit that i'm using to make
--         sure I've got the broad sketch of the design right, and there's no
--         major holes.

-- type FirstPassMonad = StateT FirstPassS Identity
--
-- type SecondPassMonad = StateT SecondPassS Symbolic
--
-- type EDGMonad = ScribeT SecondPassMonad FirstPassMonad
--
-- type ElemName = String
--
-- newtype StringID    = StringID    String deriving (Show, Read, Eq)

-- newtype PredicateID = PredicateID String deriving (Show, Read, Eq)

-- class Constrainable t => SATAbleType t where
--   type SATID  t = i | i -> t
--   type SBVRef t = j
--
--   makeVar          :: ElemName ->                  EDGMonad (SATID t)
--   makeConcreteVar  :: ElemName ->             t -> EDGMonad (SATID t,PredicateID t)
--   makeAbstractVar  :: ElemName -> Constraints t -> EDGMonad (SATID t,PredicateID t)
--   makeAmbiguousVar :: ElemName -> Ambiguous   t -> EDGMonad (SATID t,PredicateID t)
--
--   makeSBVVar       :: SATID t  -> SecondPassMonad (SBVRef t)
--   makeSBVLit       :: t        -> SecondPassMonad (SBVRef t)

-- newPredicateID  :: String      -> EDGMonad PredicateID
-- getSBVPredicate :: PredicateID -> SecondPassMonad (SBV Bool)
--
-- makeStringVar :: ElemName -> EDGMonad StringID
-- makeStringVar n = do
--   id <- makeNewStringID n
--   returnAnd id $ do
--     makeNewSBVInt id
--
-- addConstantString :: String -> EDGMonad StringID
-- addConstantString s = do
--   id <- makeStringVar
--   addToStringDB s
--   returnAnd id $ do
--     sbvVar <- getStringVar id
--     let cInt = getStringDBID s
--     constraint $ sbvVar .== literal cInt
--
-- addAbstractString :: (Constraints String) -> EDGMonad StringID
-- addAbstractString v = do
--   id <- makeStringVar
--   mapM_ addToStringDB $ getAllStrings v
--   returnAnd id $ do
--     sbvVar <- getStringVar id
--     case v of
--       SCBottom -> return ()
--       SCOneOf (OneOf s) -> do
--         ids   <- mapM getStringDBID s
--         terms <- mapM (\ i -> sbvVar .== literal i) ids
--         constraint $ foldl (.||) terms
--       SCNoneOf (NoneOf s) -> do
--         ids   <- mapM getStringDBID s
--         terms <- mapM (\ i -> sbvVar ./= literal i) ids
--         constraint $ foldl (.&&) terms
--
-- addAmbiguousString :: Ambiguous String -> EDGMonad StringID
-- addAmbiguousString Impossible = error "should not happen"
-- addAmbiguousString (Abstract c) = addAbstractString c
-- addAmbiguousString (Concrete v) = addConcreteString v


