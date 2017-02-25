
-- | The catch-all place to keep the many isntance declarations that go with
--   the EDGMonad classes.
--
--   TODO :: Reorganize the whole thing to make sure file structure is vaguely
--           sensible. Each major type should probably get its own module
--           with the central one collecting and re-exportin everything
--
module EDG.EDGInstances (
    module EDG
  , module EDG.EDGInstances.Record
  -- TODO :: figure out why the bloody fuck i have to do this manually
  --         instead of just having haskell expoert everythi
  , EDG.EDGInstances.Record.getVal
  , EDG.EDGInstances.Record.getValS
  , EDG.EDGInstances.Record.getValL
  , runGather
  , runSBVMonad
  , runEDGMonad
) where

import qualified EDG.EDGInstances.Bool    as EDG
import qualified EDG.EDGInstances.String  as EDG
import qualified EDG.EDGInstances.Float   as EDG
import qualified EDG.EDGInstances.UID     as EDG
import qualified EDG.EDGInstances.Integer as EDG
--import qualified EDG.EDGInstances.Record  as EDG
import qualified EDG.EDGInstances.Record

import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit

import Control.Monad.MonadSymbolic

import Data.SBV (
  Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  )
import qualified Data.SBV as S

import Data.IORef
--
import Control.Monad.Scribe
-- import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class
--
-- import Control.Lens.TH
--
-- import Algebra.PartialOrd
-- import Algebra.Lattice
--
-- import Algebra.Constrainable
-- import Algebra.AsPredicate

import EDG.Library.Types

import EDG.EDGMonad
import EDG.EDGDatatype

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

