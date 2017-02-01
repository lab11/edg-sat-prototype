
-- | A bunch of basic types and their corresponding kinds as specified by
--   their instances of GenOrd
module EDG.Library.Types where


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
import EDG.Library.Types.Bool
import EDG.Library.Types.Float
import EDG.Library.Types.Integer
import EDG.Library.Types.String
import EDG.Library.Types.UID
import EDG.Library.Types.Record
import EDG.Library.Types.TypeVal
-- data LibType = ...



-- TODO:
--  - IDs
--  - Integers
--  - Reals
--  - Sets
--  - Records
