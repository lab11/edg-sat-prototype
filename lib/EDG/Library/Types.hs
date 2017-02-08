
-- | A bunch of basic types and their corresponding kinds as specified by
--   their instances of GenOrd
module EDG.Library.Types (
  module LibType
) where


import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate
import Algebra.Constrainable

import EDG.Predicates

import EDG.Classes.Normalizable

import Algebra.Lattice
import Algebra.PartialOrd

import Control.Newtype
import Control.Newtype.Util

import Control.Applicative

import GHC.Exts

-- The Subtypes we're going to be working with.
import EDG.Library.Types.Bool    as LibType
import EDG.Library.Types.Float   as LibType
import EDG.Library.Types.Integer as LibType
import EDG.Library.Types.String  as LibType
import EDG.Library.Types.UID     as LibType
import EDG.Library.Types.Record  as LibType
import EDG.Library.Types.TypeVal as LibType
-- data LibType = ...



-- TODO:
--  - IDs
--  - Integers
--  - Reals
--  - Sets
--  - Records
