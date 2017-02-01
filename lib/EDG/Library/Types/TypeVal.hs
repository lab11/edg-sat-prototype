
{-# LANGUAGE UndecidableInstances #-}

-- | This module defined both the typeVal that encapsulates all the other
--   partial types we've got handy, as well as the kindVal that will be useful
--   when we're working with lots of random types and want to make them look
--   nicer.
module EDG.Library.Types.TypeVal where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Fix -- newtype Fix f = Fix {unFix :: f (Fix f)}

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

-- | This is the basic TypeVal', which disambiguates between each of the types
--   we have handy, each constructor can also act as a value level witness if
--   we need one.
--
--   Like the record, this is parameterized so that we can get the fixed point
--   in EDG.Library.Types where all the neccesary information is lying around
--   in one place.
--
data TypeVal' a where
  TVBool   :: Bool        -> TypeVal' a
  TVFloat  :: Float       -> TypeVal' a
  TVInt    :: Integer     -> TypeVal' a
  TVString :: String      -> TypeVal' a
  TVUID    :: UID Integer -> TypeVal' a
  TVRecord :: Record' a   -> TypeVal' a
  deriving (Show,Read,Eq)

type TypeVal = Fix TypeVal'

type Record = Record' TypeVal

-- | And this is the corresponding type for the constraints over a TypedValue.
--   Same deal with the parameterization.
--
--   I'm getting the constraints here rather that just typing it out because
--   it prevents user error if we change the type of some constraint.
--
--   TODO :: Feh, I'm seriously hating having to put all these explicit bottoms
--           and tops in all of our constraint dataTypes, they
data TypeCons' a where
  TCBool   :: (Constraints Bool         ) -> TypeCons' a
  TCFloat  :: (Constraints Float        ) -> TypeCons' a
  TCInt    :: (Constraints Integer      ) -> TypeCons' a
  TCString :: (Constraints String       ) -> TypeCons' a
  TCUID    :: (Constraints (UID Integer)) -> TypeCons' a
  TCRecord :: (Constraints (Record' (PredDom a))) -> TypeCons' a
  TCBottom :: TypeCons' a
  TCTop    :: TypeCons' a

type TypeCons = Fix TypeCons'

-- Again, this is where we need UndecidableInstances, because the
-- `Constraints t` is no smaller than `TypeCons' t`, but the cycle will
-- terminate once you hit the instance for the fixed point operator, since
-- that won't introduce any additional constraints.
deriving instance (
    Constrainable t
  , Eq t
  , Eq (Constraints t)
  , Eq (PredDom t)
  ) => Eq   (TypeCons' t)
-- deriving instance (      Constrainable t, Eq   t,Eq   (Constraints t)) => Eq   (TypeCons' t)
-- deriving instance (Eq t, Constrainable t, Show t,Show (Constraints t)) => Show (TypeCons' t)
-- deriving instance (Eq t, Constrainable t, Read t,Read (Constraints t)) => Read (TypeCons' t)

-- deriving instance (Read (f (Fix f))) => Read (Fix f)
--
-- instance AsPredicate (TypeCons' a) where
--   type PredicateDomain (TypeCons' a) = TypeVal' a
--
--   asPredicate (TCBool   c) (TVBool   b) = asPredicate c b
--   asPredicate (TCFloat  c) (TVFloat  b) = asPredicate c b
--   asPredicate (TCInt    c) (TVInt    b) = asPredicate c b
--   asPredicate (TCString c) (TVString b) = asPredicate c b
--   asPredicate (TCUID    c) (TVUID    b) = asPredicate c b
--   asPredicate (TCRecord c) (TVRecord b) = asPredicate c b
--   asPredicate (TCBottom  ) _            = True
--   asPredicate _            _            = False
--
-- instance AsPredicate TypeCons where
--   type PredicateDomain TypeCons = TypeVal
--
--   asPredicate (Fix c) (Fix v) = asPredicate c v
--
-- instance SATAblePredicate (TypeCons' a) where
--   isSAT (TCBool   c) = isSAT c
--   isSAT (TCFloat  c) = isSAT c
--   isSAT (TCInt    c) = isSAT c
--   isSAT (TCString c) = isSAT c
--   isSAT (TCUID    c) = isSAT c
--   isSAT (TCRecord c) = isSAT c
--   isSAT TCBottom = True
--   isSAT TCTop = False
--
-- instance BottomPredicate (TypeCons' a) where
--   isBottom (TCBool   c) = isBottom c
--   isBottom (TCFloat  c) = isBottom c
--   isBottom (TCInt    c) = isBottom c
--   isBottom (TCString c) = isBottom c
--   isBottom (TCUID    c) = isBottom c
--   isBottom (TCRecord c) = isBottom c
--   isBottom TCBottom = True
--   isBottom TCTop = False
--
-- instance CollapseablePredicate (TypeCons' a) where
--   collapse (TCBool   c) = TVBool   <$> collapse c
--   collapse (TCFloat  c) = TVFloat  <$> collapse c
--   collapse (TCInt    c) = TVInt    <$> collapse c
--   collapse (TCString c) = TVString <$> collapse c
--   collapse (TCUID    c) = TVUID    <$> collapse c
--   collapse (TCRecord c) = TVRecord <$> collapse c
--   collapse _ = Nothing
--
-- instance (Eq a,Constrainable a) =>  LiftablePredicate (TypeCons' a) where
--   liftPredicate (TVBool   b) = TCBool   $ liftPredicate b
--   liftPredicate (TVFloat  b) = TCFloat  $ liftPredicate b
--   liftPredicate (TVInt    b) = TCInt    $ liftPredicate b
--   liftPredicate (TVString b) = TCString $ liftPredicate b
--   liftPredicate (TVUID    b) = TCUID    $ liftPredicate b
--   liftPredicate (TVRecord b) = TCRecord $ liftPredicate b
--
-- instance (Eq a,Constrainable a) => Constrainable (TypeVal' a) where
--   type Constraints (TypeVal' a) = TypeCons' a
--
-- instance (Eq a,Constrainable a, PartialOrd (Constraints a)) => PartialOrd (TypeCons' a) where
--   leq TCBottom _ = True
--   leq _ TCBottom = False
--   leq _ TCTop = True
--   leq TCTop _ = False
--   leq (TCBool   c) (TCBool   c') = c `leq` c'
--   leq (TCFloat  c) (TCFloat  c') = c `leq` c'
--   leq (TCInt    c) (TCInt    c') = c `leq` c'
--   leq (TCString c) (TCString c') = c `leq` c'
--   leq (TCUID    c) (TCUID    c') = c `leq` c'
--   leq (TCRecord c) (TCRecord c') = c `leq` c'
--   leq _ _ = False
--
-- instance (Eq a,Constrainable a, JoinSemiLattice (Ambiguous a)) => JoinSemiLattice (TypeCons' a) where
--   (\/) TCTop _ = TCTop
--   (\/) _ TCTop = TCTop
--   (\/) TCBottom a = a
--   (\/) a TCBottom = a
--   (\/) (TCBool   c) (TCBool   c') = TCBool   $ c \/ c'
--   (\/) (TCFloat  c) (TCFloat  c') = TCFloat  $ c \/ c'
--   (\/) (TCInt    c) (TCInt    c') = TCInt    $ c \/ c'
--   (\/) (TCString c) (TCString c') = TCString $ c \/ c'
--   (\/) (TCUID    c) (TCUID    c') = TCUID    $ c \/ c'
--   (\/) (TCRecord c) (TCRecord c') = TCRecord $ c \/ c'
--   (\/) _ _ = TCTop
--
-- instance (Eq a,Constrainable a, MeetSemiLattice (Ambiguous a)) => MeetSemiLattice (TypeCons' a) where
--   (/\) TCBottom _ = TCBottom
--   (/\) _ TCBottom = TCBottom
--   (/\) a TCTop = a
--   (/\) TCTop a = a
--   (/\) (TCBool   c) (TCBool   c') = TCBool   $ c /\ c'
--   (/\) (TCFloat  c) (TCFloat  c') = TCFloat  $ c /\ c'
--   (/\) (TCInt    c) (TCInt    c') = TCInt    $ c /\ c'
--   (/\) (TCString c) (TCString c') = TCString $ c /\ c'
--   (/\) (TCUID    c) (TCUID    c') = TCUID    $ c /\ c'
--   (/\) (TCRecord c) (TCRecord c') = TCRecord $ c /\ c'
--   (/\) _ _ = TCBottom
--
-- instance (Eq a,Constrainable a,JoinSemiLattice (Ambiguous a)) => BoundedJoinSemiLattice (TypeCons' a) where
--   bottom = TCBottom
--
-- instance (Eq a,Constrainable a,MeetSemiLattice (Ambiguous a)) => BoundedMeetSemiLattice (TypeCons' a) where
--   top = TCTop
--
-- -- | Same Deal with kinds, we can    tie the knot externally, and get something
-- --   more useful than just this s   imple witness for the kinds of the first
-- --   set of types.
-- data KindVal' a where
--   KVBool   :: KindVal' a
--   KVFloat  :: KindVal' a
--   KVInt    :: KindVal' a
--   KVString :: KindVal' a
--   KVUID    :: KindVal' a
--   KVRecord :: Map String a -> KindVal' a
--   deriving (Show,Read,Eq)
--
-- type KindVal = Fix KindVal'
