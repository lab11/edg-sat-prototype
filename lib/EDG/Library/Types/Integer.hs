
module EDG.Library.Types.Integer () where

-- TODO :: Convert this to use Data.IntSet from containers, so that it's a
--         more efficient representation? Mind we're currently using infinite
--         precision Integers, and moving to Ints will be somewhat limiting.
--         Though probably not for practical purposes.

import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Constrainable
import EDG.Classes.Constraints
import Data.Maybe

import Algebra.Lattice
import Algebra.PartialOrd

import Control.Newtype
import Control.Newtype.Util

import Control.Applicative

import GHC.Exts

data IntConstraints = IntConstraints {
    icOneOf         :: Maybe (Set Integer)
  , icNoneOf        :: Maybe (Set Integer)
  , icLessThanEq    :: Maybe Integer
  , icGreaterThanEq :: Maybe Integer
  } deriving (Show, Read, Eq)

instance Constrainable Integer where
  data Constraints Integer = IntCnstr IntConstraints

  validate :: Constraints Integer -> Integer -> Bool
  validate (IntCnstr (IntConstraints{..})) i
    =  all ($ i) . catMaybes $ vList

    where

      -- | The list of all the checks for validity in a form that easy to
      --   evaluate in one fell swoop
      vList :: [Maybe (Integer -> Bool)]
      vList
        = [vOneOf <$> icOneOf
          ,vNoneOf <$> icNoneOf
          ,vLessThanEq <$> icLessThanEq
          ,vGreaterThanEq <$> icGreaterThanEq]

      -- | If there's an oneOf constraint, is it being met?
      vOneOf :: Set Integer -> Integer ->  Bool
      vOneOf = flip Set.member

      -- | If there's a noneOf constraint, is it being broken?
      vNoneOf :: Set Integer -> Integer -> Bool
      vNoneOf = flip Set.notMember

      -- | Check whether the number is <= what we need
      vLessThanEq :: Integer -> Integer -> Bool
      vLessThanEq c i = i <= c

      -- | Check that we're >= to what we need
      vGreaterThanEq :: Integer -> Integer -> Bool
      vGreaterThanEq c i = i >= c

  consistent :: Constraints Integer -> Bool
  consistent = under' $ (== top) . normalize

  collapse :: Constraints Integer -> Maybe Integer
  collapse = under' (\ c -> cBounds c <|> cOneOf c <|> cNoneOf c)

    where

      -- | If the Upper and Lower bounds are equal, that's clearl the number we
      --   want, so return that.
      cBounds :: IntConstraints -> Maybe Integer
      cBounds IntConstraints{icLessThanEq = Just ub,icGreaterThanEq = Just lb}
        | ub == lb = Just ub
        | otherwise = Nothing
      cBounds _ = Nothing

      -- | If there's only a single element in the OneOf list after
      --   normalization then that's the value we want to returnEq.
      cOneOf :: IntConstraints -> Maybe Integer
      cOneOf IntConstraints{icOneOf = Just s}
        | Set.size s == 1 = Just $ Set.findMin s
        | otherwise = Nothing
      cOneOf _ = Nothing

      -- | If the none-of set has one less element than the range, then return
      --   the element it doesn't have.
      --
      --   TODO :: Implement this so that it's not a no-op.
      --
      cNoneOf :: IntConstraints -> Maybe Integer
      cNoneOf _ = Nothing


  makeConstraint :: Integer -> Constraints Integer
  makeConstraint = is

-- | Map value of IntConstraints into a subset of that type. The codomain is
--   a portion of the type where structural and value equality are identical,
--   and the mapping preserves the `validate` function.
--
--   TODO :: This isn't quite correct, since you can invert a noneOf set into
--           a one of set, and vice versa. And we don't actually remove that
--           symmetry for reasons of efficiency. Fix that, see if you can do
--           it without sacrificing efficiency. Possibly, if one set becomes
--           larger than half the range then turn it into the oppposite set.
--           A normalized value will already never have more than one so that
--           should work?
--
--   TODO :: Add a test to make sure that normalization doesn't change the
--           validation set of the set of constraints.
--
normalize :: IntConstraints -> IntConstraints
normalize = nmTop . nmNoneOfGT . nmNoneOfLT . nmOneOfNone . nmOneOfGT .  nmOneOfLT

  where

    -- | Remove all elements from the OneOf set that break the LT bound
    nmOneOfLT :: IntConstraints -> IntConstraints
    nmOneOfLT c@IntConstraints{icOneOf = Just s,icLessThanEq = Just ub}
      = c {icOneOf = Just $ Set.filter (<= ub) s}
    nmOneOfLT c = c

    -- | Remove all elements from the OneOf set that break the GT bound
    nmOneOfGT :: IntConstraints -> IntConstraints
    nmOneOfGT c@IntConstraints{icOneOf = Just s,icGreaterThanEq = Just lb}
      = c {icOneOf = Just $ Set.filter (>= lb) s}
    nmOneOfGT c = c

    -- | Remove all elements from the OneOf set that break the noneOf bound
    --   then remove the NoneOf set, since all the information is already
    --   in the oneOf set.
    nmOneOfNone :: IntConstraints -> IntConstraints
    nmOneOfNone c@IntConstraints{icOneOf = Just s,icNoneOf = Just ns}
      = c {icOneOf = Just $ s `Set.difference` ns,icNoneOf = Nothing}
    nmOneOfNone c = c

    -- | Remove all elements from the NoneOf set that break the LT bound
    nmNoneOfLT :: IntConstraints -> IntConstraints
    nmNoneOfLT c@IntConstraints{icNoneOf = Just s,icLessThanEq = Just ub}
      = c {icNoneOf = Just $ Set.filter (<= ub) s}
    nmNoneOfLT c = c

    -- | Remove all elements from the NoneOf set that break the LT bound
    nmNoneOfGT :: IntConstraints -> IntConstraints
    nmNoneOfGT c@IntConstraints{icNoneOf = Just s,icGreaterThanEq = Just lb}
      = c {icNoneOf = Just $ Set.filter (>= lb) s}
    nmNoneOfGT c = c

    -- | Check whether the elements are consistent so far, and if not replace
    --   with the unique top element.
    nmTop :: IntConstraints -> IntConstraints
    nmTop c
      | icConsistent c = c
      | otherwise      = top

    -- | Combines all the individual checks to produce a global consistency
    --   check for the integer constraint element.
    icConsistent :: IntConstraints -> Bool
    icConsistent c = all ($ c) $ list [icEmpty,icRange,icNoneOf]

    -- | If there's no empty one of set, then this is consistent.
    icEmpty :: IntConstraints -> Bool
    icEmpty IntConstraints{icOneOf = Just s} = not $ Set.null s
    icEmpty _ = True

    -- | If the greater than and less than constraints overlap then
    --   this is consistent.
    icRange :: IntConstraints -> Bool
    icRange IntConstraints{icGreaterThanEq=Just lb,icLessThanEq=Just up}
      = lb > up
    icRange _ = True

    -- | If both range bounds exist, and we're allowed to be at least of the
    --   integers in between, then this is inconsistent.
    --
    --   We just make sure that, after filtering, there aren't enough unique
    --   elements to bridge the gap between the bounds.
    icNoneOf :: IntConstraints -> Bool
    icNoneOf IntConstraints{icGreaterThanEq=Just lb,icLessThanEq=Just up,icNoneOf=Just ns}
      = fromIntegral (up - lb + 1) <= Set.size ns
    icNoneOf _ = True

instance Newtype (Constraints Integer) IntConstraints where
  pack = IntCnstr
  unpack (IntCnstr a) = a

instance Eq (Constraints Integer) where
  (==) = under2 (\ a b -> normalize a == normalize b)

instance PartialOrd IntConstraints where

  -- | a `leq` b if each of their internal constraints are induvidually
  --   less than or equal.
  leq a b
      =  leqMaybe leqOneOf  (icOneOf         a') (icOneOf         b')
      && leqMaybe leqNoneOf (icNoneOf        a') (icNoneOf        b')
      && leqMaybe leqGTEq   (icGreaterThanEq a') (icGreaterThanEq b')
      && leqMaybe leqLTEq   (icLessThanEq    a') (icLessThanEq    b')

    where

      -- Normalize the two inputs because there's too many edge cases otherwise
      a' = normalize a
      b' = normalize b

      leqOneOf :: Set Integer -> Set Integer -> Bool
      leqOneOf a b = not $ Set.isProperSubsetOf a b

      leqNoneOf :: Set Integer -> Set Integer -> Bool
      leqNoneOf = Set.isSubsetOf

      leqGTEq :: Integer -> Integer -> Bool
      leqGTEq = (<=)

      leqLTEq :: Integer -> Integer -> Bool
      leqLTEq = (>=)

instance PartialOrd (Constraints Integer) where
  leq = under2 leq

instance JoinSemiLattice IntConstraints where
  (\/) a b = normalize IntConstraints {
       icOneOf         = joinMaybe joinOneOf  (icOneOf         a) (icOneOf         b)
      ,icNoneOf        = joinMaybe joinNoneOf (icNoneOf        a) (icNoneOf        b)
      ,icGreaterThanEq = joinMaybe joinGTEq   (icGreaterThanEq a) (icGreaterThanEq b)
      ,icLessThanEq    = joinMaybe joinLTEq   (icLessThanEq    a) (icLessThanEq    b)}

   where

      joinOneOf :: Set Integer -> Set Integer -> Set Integer
      joinOneOf = Set.intersection

      joinNoneOf :: Set Integer -> Set Integer -> Set Integer
      joinNoneOf = Set.union

      joinGTEq :: Integer -> Integer -> Integer
      joinGTEq = max

      joinLTEq :: Integer -> Integer -> Integer
      joinLTEq = min

instance JoinSemiLattice (Constraints Integer) where
  (\/) a b = pack $ under2 (\/) a b

instance BoundedJoinSemiLattice IntConstraints where
  bottom = IntConstraints Nothing Nothing Nothing Nothing

instance BoundedJoinSemiLattice (Constraints Integer) where
  bottom = pack $ bottom

-- TODO :: The instance of meet over a set of integer constraints. This is
--         used as a generalize operation, to fund supersets where possible.

instance MeetSemiLattice IntConstraints where
  (/\) a b = undefined

instance MeetSemiLattice (Constraints Integer) where
  (/\) a b = pack $ under2 (/\) a b

instance BoundedMeetSemiLattice IntConstraints where
  top = bottom {icOneOf = Just Set.empty}

instance BoundedMeetSemiLattice (Constraints Integer) where
  top = pack $ top

instance OneOfConstraint Integer where
  oneOf i = pack $ bottom {icOneOf = Just $ Set.fromList i}

instance NoneOfConstraint Integer where
  noneOf i = pack $ bottom {icNoneOf = Just $ Set.fromList i}

instance GTConstraint Integer where
  greaterThan   i = pack $ bottom {icGreaterThanEq = Just $ i + 1}
  greaterThanEq i = pack $ bottom {icGreaterThanEq = Just i}

instance LTConstraint Integer where
  lessThan   i = pack $ bottom {icLessThanEq = Just $ i - 1}
  lessThanEq i = pack $ bottom {icLessThanEq = Just i}

-- | Used along with the above contraint classes to allow for defining an
--   constraints as a list of things. As in the following example.
--
--   > test :: Constraints Integer
--   > test = [oneOf [2,3,4], noneOf [2,3], greaterThan 4]
--
instance IsList (Constraints Integer) where
  type Item (Constraints Integer) = Constraints Integer
  fromList = foldr (\/) bottom
  toList t = [t]

-- TODO :: Whenever you get around to it, rewrite the show and read instances
--         so that they use the above list syntax.

deriving instance Show (Constraints Integer)
deriving instance Read (Constraints Integer)
