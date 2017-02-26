
module EDG.ElemIncludes (
    module Newtype
  , module Ether
  , module MonadSymbolic
  , module Identity
  , module IO
  , module Scribe
  , module Lens
  , module Algebra
  , module EDG
  , module Expression
  , pPrint
) where

-- import Data.EqMap (EqMap)
-- import qualified Data.EqMap as EqMap
-- import Data.Bimap (Bimap)
-- import qualified Data.Bimap as Bimap
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set

import qualified Text.Pretty.Simple as P

import qualified Control.Newtype as Newtype

import qualified Control.Monad.Ether.Implicit as Ether
import qualified Control.Lens.Ether.Implicit as Ether

import qualified Control.Monad.MonadSymbolic as MonadSymbolic
-- import Data.SBV (
--   Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
--   )
-- import qualified Data.SBV as S
--
-- import Data.IORef
--
import qualified Control.Monad.Scribe as Scribe
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.IO.Class as IO

import qualified Control.Newtype.Util as Util

import qualified Control.Lens.TH as Lens
--
-- import Algebra.PartialOrd
-- import Algebra.Lattice
--
import qualified Algebra.Constrainable as Algebra
import qualified Algebra.AsPredicate as Algebra

import qualified EDG.Expression as Expression
-- import EDG.Library.Types
-- import EDG.Predicates
--
import qualified EDG.EDGMonad as EDG
import qualified EDG.EDGDatatype as EDG
--
-- import EDG.EDGInstances
--

pPrint :: (IO.MonadIO m, Show a) => a -> m ()
pPrint = P.pPrintOpt P.defaultOutputOptionsDarkBg{P.outputOptionsIndentAmount=2}


