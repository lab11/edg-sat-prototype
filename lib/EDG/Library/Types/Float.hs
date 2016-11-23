
module EDG.Library.Types.Float () where

import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Constrainable
import EDG.Classes.Constraints
import Data.Maybe

data FloatConstraints = FloatConstraints {
    fcOneOf :: Maybe (Set Float)
  , fcGreaterThan :: Maybe (Float,IsInclusive)
  , fcLessThan :: Maybe (Float,IsInclusive)
  } deriving (Show, Read, Eq)

instance Constrainable Float where
  data Constraints Float = FltCnstr FloatConstraints

  -- | Given a set of constraints on a value, and a value, return whether the
  --   value matches the set of constraints.
  validate :: Constraints Float -> Float -> Bool
  validate = undefined

  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  consistent :: Constraints Float -> Bool
  consistent = undefined

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  collapse :: Constraints Float -> Maybe Float
  collapse = undefined

  -- | Attempt to lift a concrete value into a constraint that covers it. This
  --   is used to allow us to more easily define the MeetSemiLattice instances
  --   for an Ambiguous t
  makeConstraint :: Float -> Constraints Float
  makeConstraint = undefined

-- | Map value of FloatConstraints into a subset of that type. The codomain is
--   a portion of the type where structural and value equality are identical,
--   and the mapping preserves the `validate` function.
--
--   TODO :: Add a test to make sure that normalization doesn't change the
--           validation set of the set of constraints.
--
normalize :: FloatConstraints -> FloatConstraints
normalize = undefined
-- normalize = nmTop . nmNoneOfGT . nmNoneOfLT . nmOneOfNone . nmOneOfGT .  nmOneOfLT
--
--   where
--
--     -- | Remove all elements from the OneOf set that break the LT bound
--     nmOneOfLT :: IntConstraints -> IntConstraints
--     nmOneOfLT c@IntConstraints{icOneOf = Just s,icLessThanEq = Just ub}
--       = c {icOneOf = Just $ Set.filter (<= ub) s}
--     nmOneOfLT c = c
--
--     -- | Remove all elements from the OneOf set that break the GT bound
--     nmOneOfGT :: IntConstraints -> IntConstraints
--     nmOneOfGT c@IntConstraints{icOneOf = Just s,icGreaterThanEq = Just lb}
--       = c {icOneOf = Just $ Set.filter (>= lb) s}
--     nmOneOfGT c = c
--
--     -- | Remove all elements from the OneOf set that break the noneOf bound
--     --   then remove the NoneOf set, since all the information is already
--     --   in the oneOf set.
--     nmOneOfNone :: IntConstraints -> IntConstraints
--     nmOneOfNone c@IntConstraints{icOneOf = Just s,icNoneOf = Just ns}
--       = c {icOneOf = Just $ s `Set.difference` ns,icNoneOf = Nothing}
--     nmOneOfNone c = c
--
--     -- | Remove all elements from the NoneOf set that break the LT bound
--     nmNoneOfLT :: IntConstraints -> IntConstraints
--     nmNoneOfLT c@IntConstraints{icNoneOf = Just s,icLessThanEq = Just ub}
--       = c {icNoneOf = Just $ Set.filter (<= ub) s}
--     nmNoneOfLT c = c
--
--     -- | Remove all elements from the NoneOf set that break the LT bound
--     nmNoneOfGT :: IntConstraints -> IntConstraints
--     nmNoneOfGT c@IntConstraints{icNoneOf = Just s,icGreaterThanEq = Just lb}
--       = c {icNoneOf = Just $ Set.filter (>= lb) s}
--     nmNoneOfGT c = c
--
--     -- | Check whether the elements are consistent so far, and if not replace
--     --   with the unique top element.
--     nmTop :: IntConstraints -> IntConstraints
--     nmTop c
--       | icConsistent c = c
--       | otherwise      = top
--
--     -- | Combines all the individual checks to produce a global consistency
--     --   check for the integer constraint element.
--     icConsistent :: IntConstraints -> Bool
--     icConsistent c = all ($ c) $ list [icEmpty,icRange,icNoneOf]
--
--     -- | If there's no empty one of set, then this is consistent.
--     icEmpty :: IntConstraints -> Bool
--     icEmpty IntConstraints{icOneOf = Just s} = not $ Set.null s
--     icEmpty _ = True
--
--     -- | If the greater than and less than constraints overlap then
--     --   this is consistent.
--     icRange :: IntConstraints -> Bool
--     icRange IntConstraints{icGreaterThanEq=Just lb,icLessThanEq=Just up}
--       = lb > up
--     icRange _ = True
--
--     -- | If both range bounds exist, and we're allowed to be at least of the
--     --   integers in between, then this is inconsistent.
--     --
--     --   We just make sure that, after filtering, there aren't enough unique
--     --   elements to bridge the gap between the bounds.
--     icNoneOf :: IntConstraints -> Bool
--     icNoneOf IntConstraints{icGreaterThanEq=Just lb,icLessThanEq=Just up,icNoneOf=Just ns}
--       = fromIntegral (up - lb + 1) <= Set.size ns
--     icNoneOf _ = True


instance Newtype (Constraints Float) FloatConstraints where
  pack = FltCnstr
  unpack (FltCnstr a) = a

instance Eq (Constraints Float) where
  (==) = under2 (\ a b -> normalize a == normalize b)

instance PartialOrd IntConstraints where
  leq a b = undefined
--   -- | a `leq` b if each of their internal constraints are induvidually
--   --   less than or equal.
--   leq a b
--       =  leqMaybe leqOneOf  (icOneOf         a') (icOneOf         b')
--       && leqMaybe leqNoneOf (icNoneOf        a') (icNoneOf        b')
--       && leqMaybe leqGTEq   (icGreaterThanEq a') (icGreaterThanEq b')
--       && leqMaybe leqLTEq   (icLessThanEq    a') (icLessThanEq    b')
--
--     where
--
--       -- Normalize the two inputs because there's too many edge cases otherwise
--       a' = normalize a
--       b' = normalize b
--
--       leqOneOf :: Set Integer -> Set Integer -> Bool
--       leqOneOf a b = not $ Set.isProperSubsetOf a b
--
--       leqNoneOf :: Set Integer -> Set Integer -> Bool
--       leqNoneOf = Set.isSubsetOf
--
--       leqGTEq :: Integer -> Integer -> Bool
--       leqGTEq = (<=)
--
--       leqLTEq :: Integer -> Integer -> Bool
--       leqLTEq = (>=)
--
--       -- | Lift the lower order checks into a higher order one, since `Nothing`
--       --   is bottom for this constraint set.
--       leqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
--       leqMaybe _ Nothing  _         = True
--       leqMaybe _ _        Nothing   = False
--       leqMaybe f (Just a) (Just b ) = f a b

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

      joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
      joinMaybe f (Just a) (Just b) = Just $ f a b
      joinMaybe _ a b = a <|> b

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

