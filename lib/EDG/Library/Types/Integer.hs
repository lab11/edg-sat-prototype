
module EDG.Library.Types.Integer where

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

-- | The canonical constraint type for Integers.
data IntCons
  -- | All other constraint types fold into the OneOf
  = ICOneOf (OneOf Integer)
  -- | A combination of the other likely constraints over integers
  --   which combine in various convinient ways
  | ICOther {
      none  :: Maybe (NoneOf Integer)
    , lower :: Maybe (LowerBound Integer)
    , upper :: Maybe (UpperBound Integer)
    }
  -- | Universal predicate
  | ICBottom
  deriving (Show, Read)

instance Normalizable (PredEq IntCons) where
  normalize = over PredEq icNorm

instance Eq IntCons where
  (==) a b = (icNorm a) `icStructuralEq` (icNorm b)

instance PartialOrd IntCons where
  leq a b = nleq nra nrb
    where
      nra = icNorm a
      nrb = icNorm b
      nleq ICBottom _ = True
      nleq _ ICBottom = False
      nleq (ICOneOf a) (ICOneOf b) = a `leq` b
      -- TODO :: This case is wrong, fix it
      --
      -- It's possible to have a really spare OneOf shadow a really tiny noneOf
      -- over its entire range. Something like:
      --
      -- a =      >--o---<
      -- b = - -   -- ---  -   -  -       --         -       ---     -
      --
      -- Likely slow-but-correct solution is pick the set with the smallest
      -- total number of elements and just check each one.
      nleq (ICOneOf _) (ICOther _ _ _) = False
      -- TODO :: So is this one, for a similar reason as the above. False is
      --         just a placeholder that shouldn't mess things up too much in
      --         this part of the project.
      nleq (ICOther _ _ _) (ICOneOf _) = False
      -- TODO :: This should only compare na and nb within the overlap of
      --         (la,ua) and (lb,ub), fix this too at some point.
      nleq (ICOther na la ua) (ICOther nb lb ub) = (na `leq` nb) && (la `leq` lb) && (ua `leq` ub)

instance JoinSemiLattice IntCons where
  (\/) ICBottom a = a
  (\/) a ICBottom = a
  (\/) (ICOneOf o) a = icNorm . ICOneOf $ joinOneOf a o
  (\/) a (ICOneOf o) = icNorm . ICOneOf $ joinOneOf a o
  (\/) (ICOther na la ua) (ICOther nb lb ub)
    = icNorm $ ICOther (na \/ nb) (la \/ lb) (ua \/ ub)

instance MeetSemiLattice IntCons where
  (/\) ICBottom _ = ICBottom
  (/\) _ ICBottom = ICBottom
  (/\) (ICOneOf a) (ICOneOf b) = icNorm $ ICOneOf (a /\ b)
  (/\) (ICOneOf o) (ICOther na la ua) = icNorm $ ICOther no lo bo
    where
      -- TODO :: Fliping is woefully painful in those cases where the OneOf
      -- is pretty sparse, we should probably find a better way to do this
      -- in the longer term.
      (nb,lb,ub) = flipOneOf o
      (no,lo,bo) = (Just nb,Just lb,Just ub) /\ (na,la,ua)
  (/\) (ICOther na la ua) (ICOneOf o) = icNorm $ ICOther no lo bo
    where
      -- TODO :: Split thisout into its own function and call it with flipped
      -- parameters :V don't leave duplicated code lying around.
      (nb,lb,ub) = flipOneOf o
      (no,lo,bo) = (Just nb,Just lb,Just ub) /\ (na,la,ua)
  (/\) (ICOther na la ua) (ICOther nb lb ub) = icNorm $ ICOther no lo bo
    where (no,lo,bo) = (na,la,ua) /\ (nb,lb,ub)

instance BoundedJoinSemiLattice IntCons where
  bottom = ICBottom

instance BoundedMeetSemiLattice IntCons where
  top = ICOneOf top

instance AsPredicate IntCons where
  type PredicateDomain IntCons = Integer
  asPredicate (ICOneOf o) = asPredicate o
  asPredicate (ICOther n l u) = asPredicate (n,l,u)

instance SATAblePredicate IntCons where
  isSAT (ICOneOf o) = isSAT o
  isSAT (ICBottom) = True
  isSAT i = case icNorm i of
    (ICOneOf o) -> isSAT o
    _ -> True

instance CollapseablePredicate IntCons where
  collapse (ICOneOf o) = collapse o
  collapse _ = Nothing

instance LiftablePredicate IntCons where
  liftPredicate i = ICOneOf $ liftPredicate i

instance BottomPredicate IntCons where
  isBottom i = case icNorm i of
    ICBottom -> True
    _ -> False

instance Constrainable Integer where
  type Constraints Integer = IntCons

instance OneOfConstraint IntCons where
  oneOf i = icNorm . ICOneOf $ oneOf i

instance NoneOfConstraint IntCons where
  noneOf i = icNorm . (\ n -> ICOther n Nothing Nothing) $ noneOf i

instance GTConstraint IntCons where
  greaterThan   i = icNorm . (\ l -> ICOther Nothing l Nothing) $ greaterThan i
  greaterThanEq i = icNorm . (\ l -> ICOther Nothing l Nothing) $ greaterThanEq i

instance LTConstraint IntCons where
  lessThan   i = icNorm . (\ u -> ICOther Nothing Nothing u) $ lessThan i
  lessThanEq i = icNorm . (\ u -> ICOther Nothing Nothing u) $ lessThanEq i

-- | Used along with the above contraint classes to allow for defining an
--   constraints as a list of things. As in the following example.
--
--   > test :: IntCons
--   > test = [oneOf [2,3,4], noneOf [2,3], greaterThan 4]
--
instance IsList IntCons where
  type Item IntCons = IntCons
  fromList = foldr (\/) bottom
  toList t = [t]

-- | Structural Equality over the intCons type, which is different from
--   predicate equality for non-normalized values of IntCons
--
-- TODO :: In the long term this should probably just use the `eq` typeclass
-- with our own class hierarchy that captures predicate equality, predicate
-- partial orderings, predicate joins, etc..
icStructuralEq :: IntCons -> IntCons -> Bool
icStructuralEq (ICOneOf a)     (ICOneOf b)        = a == b
icStructuralEq (ICOther n l u) (ICOther n' l' u') = (l == l') && (u == u') && (n == n')
icStructuralEq ICBottom        ICBottom           = True
icStructuralEq _               _                  = False

-- | Normalize the representation of an IntCons such that structural equality
--   is the same as predicate equality in the normalized subspace.
--
--  TODO :: Double check that this is correct. If it isn't acting as an
--  idempotent projection into a subspace where structural equality is the
--  same as predicate equality, this entire module is broken! Everything here
--  depends on that property.
--
icNorm :: IntCons -> IntCons
icNorm i@ICBottom        = i
icNorm i@(ICOneOf _)     = normOneOf i
  where
    -- | Given an instance of ICOneOf, check that it's not overly large, if so
    --   convert it to an instance of ICOther.
    normOneOf :: IntCons -> IntCons
    normOneOf i@(ICOneOf (OneOf s))
      | Set.null s = i
      -- TODO :: replace this with a call to `flipOneOf`
      | tooLarge   = icNorm (ICOther (noneOf inverseList) (greaterThanEq sMin) (lessThanEq sMax))
      | otherwise  = i
      where

        sMin :: Integer
        sMin = Set.findMin s

        sMax :: Integer
        sMax = Set.findMax s

        sSize :: Integer
        sSize = toInteger $ Set.size s

        -- | Is the set larger than 1/2 of its range?
        --
        -- TODO :: Split this function out into it's thing, so it can be used
        -- in both this and `normNoneOf`, we'll need to tweak the parameters
        -- to minimize the amount of effort taken and doing it in one place is
        -- better.
        --
        tooLarge :: Bool
        tooLarge = (sSize > 20) && (range `div` 2) <= sSize
          where range = sMax - sMin + 1

        --   TODO :: Make this use the flip* functions in EDG.Predicates
        --   instead of duplicating code.
        inverseList = [i | i <- [sMin .. sMax], Set.notMember i s]
    normOneOf i = i

-- TODO :: Seriously though, this one is tough, I think I'm removing all the
-- symmetries in the space but I'm by no means sure. It might also be worth
-- splitting much of this code out into EDG.Predicates if other constraints
-- use it.
icNorm i@(ICOther _ _ _)
  = normNoneOf . normDelNoneOf . normFilterNone . normTightenUB . normTightenLB . normUB . normLB $ i
  where

    -- | Normalize the lower bound if needed
    normLB i@ICOther{lower = Just lb} = i{lower = Just $ normalizeEnumLB lb}
    normLB i = i

    -- | Normalize the lower bound if needed
    normUB i@ICOther{upper = Just ub} = i{upper = Just $ normalizeEnumUB ub}
    normUB i = i

    -- | Given instances of bottom, turns them into ICBottom
    normBottom :: IntCons -> IntCons
    normBottom ICBottom = ICBottom
    normBottom (ICOther Nothing Nothing Nothing) = ICBottom
    normBottom i = i


    -- | Given an other list, tighten the lower bounds and remove unneccesary elements
    --   from the noneOf list.
    --
    --   this requires that you have a normLB'ed predicate.
    normTightenLB :: IntCons -> IntCons
    normTightenLB i@ICOther{none = Just (NoneOf ns), lower = Just (LowerBound True lb)}
      | Set.member lb ns = normTightenLB i{none  = Just (NoneOf (Set.delete lb ns)),
                                           lower = Just (LowerBound True (succ lb))}
      | otherwise = i
    normTIghtenLB i = i

    -- | Given an other list, tighten the upper bounds and remove unneccesary elements
    --   from the noneOf list.
    --
    --   this requires that you have a normLB'ed predicate.
    normTightenUB :: IntCons -> IntCons
    normTightenUB i@ICOther{none = Just (NoneOf ns), upper = Just (UpperBound True ub)}
      | Set.member ub ns = normTightenUB i{none  = Just (NoneOf (Set.delete ub ns)),
                                           upper = Just (UpperBound True (pred ub))}
      | otherwise = i
    normTightenUB i = i

    -- | filter out elements from the NoneOf that are outside the bounds.
    normFilterNone :: IntCons -> IntCons
    normFilterNone i@ICOther{none = Just (NoneOf ns),lower,upper}
      = i{none = Just (NoneOf (Set.filter (asPredicate @(Range Integer) $ pack (lower,upper)) ns))}
    normFilterNone i = i

    -- | replace an empty NoneOf list with nothing, propagate the bottom upward.
    normDelNoneOf :: IntCons -> IntCons
    normDelNoneOf i@ICOther{none = Just (NoneOf ns),lower,upper}
      | Set.null ns = case (lower,upper) of
                        (Nothing, Nothing) -> ICBottom
                        _ -> i{none = Nothing}
      | otherwise = i
    normDelNoneOf i = i

    -- | If the NoneOf Set is too large, convert it to a oneOf Set
    --
    --   TODO :: This and `normOneOf` are very likely to have an off by one
    --           error of some sort. So find it and fix it.
    normNoneOf :: IntCons -> IntCons
    normNoneOf i@ICOther{none = Just (NoneOf ns)
                       ,lower = Just (LowerBound True lb)
                       ,upper = Just (UpperBound True ub)}
      | Set.null ns = i{none = Nothing}
      | tooLarge = ICOneOf $ oneOf [i| i <- [lb..ub], Set.notMember i ns]
      | otherwise = i
      where
        nSize :: Integer
        nSize = toInteger $ Set.size ns

        tooLarge :: Bool
        tooLarge = (range `div` 2 > sSize) || (sSize <= 20)
          where
            range = ub - lb + 1
            sSize = range - nSize
    normNoneOf i = i

