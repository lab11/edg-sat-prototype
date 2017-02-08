
-- | Instances of the SBVWrap class found in Control.Monad.MonadSymbolic
--   that allow us to have nicer wrappers for `SBV` values, that typecheck
--   more effectively.
module EDG.SBVWrap where

-- import qualified Data.SBV as S
-- import qualified Data.SBV.Internals as S
-- import qualified Data.SBV.Dynamic as S

import Control.Monad.MonadSymbolic
import EDG.Library.Types.UID

instance SBVWrap String Integer

sString :: MonadSymbolic m => String -> m (SBV String)
sString = getRep

instance SBVWrap UID Integer

sUID :: MonadSymbolic m => String -> m (SBV UID)
sUID = getRep
