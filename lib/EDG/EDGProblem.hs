

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
import EDG.EDGInstances


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
    throwUp (Left err) = error $ "Execution failed in SBV phase with error: " ++ err
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
  Left err -> error $ "Execution failed in Gather phase with error: " ++ err
  Right ((a,sbvm),gs) -> (runSBVMonad (transformState gs) sio sbvm,gs,a)

-- | Just a test problem I'll be editing a lot.
--
--   TODO ;: Change this interface so it's easier to get the RefType values
--           back out once you're done. Not sure how that'll look either.
--
testProblem :: EDGMonad (Ref Float, Ref Float, Ref Bool)
testProblem = do
  b1 <- refAbstract @Float "b1" [greaterThan 3.2, lessThan 12.4]
  b2 <- refAbstract @Float "b2" [greaterThan 10.2, lessThan 15.4]
  b3 <- b1 .== b2
  constrain $ b3
  return (b1,b2,b3)

-- | What `main` in "app/Main.hs" calls.
runTestProblem :: IO ()
runTestProblem = do
  ss <- newIORef (undefined :: SBVState)
  let (symb,gs,(b1,b2,b3)) = runEDGMonad (Just ss) testProblem
  sol <- S.satWith S.defaultSMTCfg{S.verbose = False} symb
  print sol
  ss' <- readIORef ss
  print ss'
  let decSt = buildDecodeState gs ss'
  print $ extract decSt sol b1
  print $ extract decSt sol b2
  print $ extract decSt sol b3


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


