
{-# LANGUAGE UndecidableInstances #-}

-- This is only neccesary for the various Show/Read instances, and those
-- are acyclic.

module EDG.Library.Types.Record where

import Data.Map (Map)
import qualified Data.Map as Map

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

-- | The basic type of a record in this system, records store a number of
--   K/V pairs, where the keys are strings and values are whatever
--
--   This is the parameterized version, the full version will be defined in
--   EDG.Library.Types.TypeRep.
newtype Record' a = Record' {rMap :: Map String a}
  deriving (Eq,Show,Read)

-- | The cosntraint type on records, more or less the same as before, but
--   with a few nice
data RecordCons a where
  RCAmbig  :: (Eq a,Constrainable a)
             => {rcMap :: Map String (Ambiguous a)}
             -> RecordCons a
  RCBottom :: RecordCons a
  RCTop    :: RecordCons a

-- Need a standalone instances here because of the extra constraint needed
-- on the constraints of `a`. Those can't be automatically derived.
--
-- These instances are also why we need UndecidableInstances, but this should
-- be fine, we'll see if it chokes I suppose. It might be a problem if the
-- system doesn't notice that a recordCons might require itself to be true.
-- Coinduction is fun :V
--
deriving instance (      Constrainable a, Eq   a,Eq   (Constraints a)) => Eq   (RecordCons a)
deriving instance (Eq a, Constrainable a, Show a,Show (Constraints a)) => Show (RecordCons a)
deriving instance (Eq a, Constrainable a, Read a,Read (Constraints a)) => Read (RecordCons a)

instance AsPredicate a => AsPredicate (RecordCons a) where
  type PredicateDomain (RecordCons a) = Record' a

  -- | In order to match a predicate properly, all of the fields need to exist
  --   and the corresponding values have to be within the specified
  --   constraints.
  asPredicate :: RecordCons a -> Record' a -> Bool
  asPredicate RCTop    _ = False
  asPredicate RCBottom _ = True
  asPredicate RCAmbig{..} Record'{..} = rcSubR && rSubRC
    where
      -- | Do all the keys in R show up in RC?
      rcSubR :: Bool
      rcSubR = Map.isSubmapOfBy (\ _ _ -> True) rMap rcMap
      -- | Do all the keys in RC show up in R? and are their corresponding
      --   values within the domain of the RC predicates?
      rSubRC :: Bool
      rSubRC = Map.isSubmapOfBy asPredicate rcMap rMap


