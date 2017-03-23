{-# LANGUAGE UndecidableInstances #-}

module EDG.Library.Types.Record where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

-- | The basic type of a record in this system, records store a number of
--   K/V pairs, where the keys are strings and values are whatever
--
--   This is the parameterized version, the full version will be defined in
--   EDG.Library.Types. We keep the parameterized partial version
--   here so that the modules are more organized.
newtype Record' v = Record' {rMap :: Map String v}
  deriving (Eq,Show,Read)

instance Newtype (Record' v) (Map String v) where
  pack = Record'
  unpack = rMap

-- | The cosntraint type on records, more or less t
data RecordCons t where
  RCAmbig  :: (Eq t,Constrainable t)
             => {rcMap :: Map String (Ambiguous t)}
             -> RecordCons t
  RCBottom :: RecordCons t
  RCTop    :: RecordCons t

-- Need a standalone instances here because of the extra constraint needed
-- on the constraints of `a`. Those can't be automatically derived.
--
-- These instances are also why we need UndecidableInstances, but this should
-- be fine, we need a newtype in order to get to the fixed point, but that
-- causes the search to halt withou every cycling.
--
deriving instance (      Constrainable t, Eq   t,Eq   (Constraints t)) => Eq   (RecordCons t)
deriving instance (Eq t, Constrainable t, Show t,Show (Constraints t)) => Show (RecordCons t)
deriving instance (Eq t, Constrainable t, Read t,Read (Constraints t)) => Read (RecordCons t)

instance AsPredicate (RecordCons t) where
  type PredicateDomain (RecordCons t) = Record' t

  -- | In order to match a predicate properly, all of the fields need to exist
  --   and the corresponding values have to be within the specified
  --   constraints.
  asPredicate :: RecordCons t -> Record' t -> Bool
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

instance SATAblePredicate (RecordCons t) where
  isSAT RCBottom = True
  isSAT RCTop = False
  -- | It's every element satisfiable?
  isSAT RCAmbig{..} = all isSAT rcMap

instance CollapseablePredicate (RecordCons t) where
  collapse RCTop = Nothing
  collapse RCBottom = Nothing
  -- Collapse every element, if that works on everything, pull the Maybe out
  -- and pack the element into a Record' again.
  collapse RCAmbig{..} = Record' <$> mapM collapse rcMap

instance (Eq t,Constrainable t) => LiftablePredicate (RecordCons t) where
  -- | Pull the map out, lift all the internal values, and wrap it up in a
  --   RecordCons again.
  liftPredicate = RCAmbig . Map.map liftPredicate . rMap

instance BottomPredicate (RecordCons t) where
  isBottom RCBottom = True
  isBottom RCTop    = False
  isBottom RCAmbig{..} = all isBottom rcMap

instance (Eq t,Constrainable t) => Constrainable (Record' t) where
  type Constraints (Record' t) = RecordCons t

-- | Check that two maps have the exact same set of keys.
keyEq :: Eq a => Map a b -> Map a c -> Bool
keyEq a b = Map.keysSet a == Map.keysSet b


-- | We're specifically requiring that *all* the fields of the record match in
--   order for two RecordCons to be related to each other.
--
--   That way we limit the height of the RecordCons lattice as long as the
--   height of the lattices for each of the sub-elements is also limited. This
--   means we get an instance of CollapseablePredicate that's actually useful.
--
--   TODO :: Revisit the above decision, is is okay to have it the other way?
--           It might make it easier to work with as we do various
--           transformations on incomplete designs.
--
--           Keep in mind this will make (\/) and (/\) more complex than the
--           nice piecewise version we can use at the moment.
--
instance (
    Eq t
  , Eq (Constraints t)
  , Constrainable t
  , PartialOrd (Ambiguous t)
  ) => PartialOrd (RecordCons t) where

  leq RCBottom _ = True
  leq _ RCBottom = False
  leq _ RCTop = True
  leq RCTop _ = False
  leq (RCAmbig rcMap) (RCAmbig rcMap') = ke && vl
    where
      ke = keyEq rcMap rcMap'
      vl = Map.isSubmapOfBy (leq) rcMap rcMap'

instance (Eq t, Constrainable t, JoinSemiLattice (Ambiguous t)) => JoinSemiLattice (RecordCons t) where
  (\/) a _ | unSAT a = error ("value is unSat")
  (\/) _ a | unSAT a = error ("value is unSat")
  (\/) RCBottom a = a
  (\/) a RCBottom = a
  -- We're good if keys match, and the joins of each element are also not top.
  -- otherwise this is a failure.
  (\/) a@(RCAmbig aMap) b@(RCAmbig bMap)
    | keyEq aMap bMap && isSAT joinRC = joinRC
    | otherwise = RCTop
    where
      joinRC = RCAmbig $ Map.unionWith (\/) aMap bMap

instance (Eq t,Constrainable t, MeetSemiLattice (Ambiguous t)) => MeetSemiLattice (RecordCons t) where
  (/\) RCBottom _ = RCBottom
  (/\) _ RCBottom = RCBottom
  (/\) RCTop a = a
  (/\) a RCTop = a
  (/\) a@(RCAmbig aMap) b@(RCAmbig bMap)
    | keyEq aMap bMap && (not $ isBottom meetRC) = meetRC
    | otherwise = RCBottom
    where
      meetRC = RCAmbig $ Map.unionWith (/\) aMap bMap

instance (Eq t, Constrainable t,JoinSemiLattice (Ambiguous t)) => BoundedJoinSemiLattice (RecordCons t) where
  bottom = RCBottom

instance (Eq t, Constrainable t,MeetSemiLattice (Ambiguous t)) => BoundedMeetSemiLattice (RecordCons t) where
  top = RCTop

-- We're leaving the IsList instance out on purpose. It'll be defined in
-- EDG.Library.Types, where we'll have enough information to give it a more
-- useful instance.
