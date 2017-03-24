
module EDG.Library.Types.Float where

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

import GHC.Generics
import Control.DeepSeq

import Control.Newtype
import Control.Newtype.Util

import Control.Applicative

import Debug.Trace

import GHC.Exts

-- | The Datatype for constraints over floats. This one is a bit iffy because
--   of floating point error, NaN, infty, and -infty, but we'll just have to
--   be careful I suppose.
data FloatCons
  = FCOneOf (OneOf Float)
  | FCRange (Range Float)
  | FCBottom
  deriving (Show,Read,Eq, Generic, NFData)

instance AsPredicate FloatCons where
  type PredicateDomain FloatCons = Float
  asPredicate (FCOneOf a) = asPredicate a
  asPredicate (FCRange a) = asPredicate a
  asPredicate (FCBottom ) = const True

instance SATAblePredicate FloatCons where
  isSAT (FCOneOf a) = {- trace (show a ++ "<3") $ -} isSAT a
  isSAT (FCRange a) = {- trace (show a ++ "<4") $ -} isSAT a
  isSAT (FCBottom)  = True

instance CollapseablePredicate FloatCons where
  collapse (FCOneOf a) = collapse a
  collapse (FCRange a) = collapse a
  collapse (FCBottom ) = Nothing

instance LiftablePredicate FloatCons where
  liftPredicate = FCOneOf . liftPredicate

instance BottomPredicate FloatCons where
  isBottom FCBottom = True
  isBottom _ = False

instance Constrainable Float where
  type Constraints Float = FloatCons

instance PartialOrd FloatCons where
  leq FCBottom _ = True
  leq _ FCBottom = False
  leq (FCOneOf a) (FCOneOf b) = a `leq` b
  leq (FCRange a) (FCRange b) = a `leq` b
  -- TODO :: I'm just treating floats as a real type for now, so there's no
  -- way a finite set of reals can cover any non-empty measure. This might
  -- be worth fixing, but it might also just be a good idea to use a Rational
  -- type or algebraic reals.
  leq (FCOneOf s) (FCRange a)
    | unSAT a = True
    | otherwise = False
  leq (FCRange a) (FCOneOf s)
    | unSAT s = True
    | otherwise = False

instance JoinSemiLattice FloatCons where
  (\/) FCBottom a = {- trace (show a ++ " |- ") -} a
  (\/) a FCBottom = {- trace (show a ++ " |- ") -} a
  (\/) (FCOneOf a) p = {- trace (show a ++ " |1 " ++  show p ++ " :> " ++ show (joinOneOf p a)) $ -} FCOneOf $ joinOneOf p a
  (\/) p (FCOneOf a) = {- trace (show a ++ " |2 " ++  show p ++ " :> " ++ show (joinOneOf p a)) $ -}  FCOneOf $ joinOneOf p a
  (\/) (FCRange a) (FCRange b) = {- trace (show a ++ " |3 " ++ show b ++ " :> " ++ show (FCRange $ a \/ b)) $ -} FCRange $ a \/ b

instance MeetSemiLattice FloatCons where
  (/\) FCBottom _ = FCBottom
  (/\) _ FCBottom = FCBottom
  (/\) (FCOneOf a) (FCOneOf b) = FCOneOf $ a /\ b
  (/\) (FCRange a) (FCRange b) = FCRange $ a /\ b
  (/\) (FCOneOf o) (FCRange b)
    | unSAT o = FCRange b
    | otherwise = FCRange $  b /\ greaterThanEq (under' Set.findMin o) /\ lessThanEq (under' Set.findMax o)
  (/\) (FCRange b) (FCOneOf o)
    | unSAT o = FCRange b
    | otherwise = FCRange $  b /\ greaterThanEq (under' Set.findMin o) /\ lessThanEq (under' Set.findMax o)

instance BoundedJoinSemiLattice FloatCons where
  bottom = FCBottom

instance BoundedMeetSemiLattice FloatCons where
  top = FCOneOf top

instance OneOfConstraint FloatCons where
  oneOf = FCOneOf . oneOf

instance GTConstraint FloatCons where
  greaterThan = FCRange . greaterThan
  greaterThanEq = FCRange . greaterThanEq

instance LTConstraint FloatCons where
  lessThan = FCRange . lessThan
  lessThanEq = FCRange . lessThanEq

-- | This allows for declarations of types like the following
--
--   > fc :: FloatCons
--   > fc = [oneOf [12,13,14], lessThan 20]
--
--   It's a shiny way to use this language extension for a more natural
--   looking EDSL
instance IsList FloatCons where
  type Item FloatCons = FloatCons
  fromList = foldr (\/) bottom
  toList t = [t]
