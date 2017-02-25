
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
  , EDG.EDGInstances.Record.getVal
  , EDG.EDGInstances.Record.getValS
  , EDG.EDGInstances.Record.getValL
) where

import qualified EDG.EDGInstances.Bool    as EDG
import qualified EDG.EDGInstances.String  as EDG
import qualified EDG.EDGInstances.Float   as EDG
import qualified EDG.EDGInstances.UID     as EDG
import qualified EDG.EDGInstances.Integer as EDG
--import qualified EDG.EDGInstances.Record  as EDG
import qualified EDG.EDGInstances.Record

test = EDG.EDGInstances.Record.getValS
