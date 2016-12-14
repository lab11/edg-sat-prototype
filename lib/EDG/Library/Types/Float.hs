
module EDG.Library.Types.Float where

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
  validate (FltCnstr FloatConstraints{..}) i
    =  all ($ i) . catMaybes $ vList

    where

      -- | The list of all the checks for validity in a form that easy to
      --   evaluate in one fell swoop
      vList :: [Maybe (Float -> Bool)]
      vList
        = [vOneOf <$> fcOneOf
          ,vLessThan <$> fcLessThan
          ,vGreaterThan <$> fcGreaterThan]

      -- | If there's an oneOf constraint, is it being met?
      vOneOf :: Set Float -> Float ->  Bool
      vOneOf = flip Set.member

      -- | Check whether the number is <= what we need
      vLessThan :: (Float,IsInclusive) -> Float -> Bool
      vLessThan (c,True)  i = i <= c
      vLessThan (c,False) i = i <  c

      -- | Check that we're >= to what we need
      vGreaterThan :: (Float,IsInclusive) -> Float -> Bool
      vGreaterThan (c,True)  i = i >= c
      vGreaterThan (c,False) i = i >  c


  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  consistent :: Constraints Float -> Bool
  consistent = under' $ (/= top) . normalize

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  --
  --   The only case we really have to deal with is the instance where there's
  --   a single element in the OneOf set after normalization.
  collapse :: Constraints Float -> Maybe Float
  collapse = under' $ (\c -> cOneOf c <|> cRange c) . normalize

    where

      -- | If there's only a single element in the OneOf list after
      --   normalization then that's the value we want to returnEq.
      cOneOf :: FloatConstraints -> Maybe Float
      cOneOf FloatConstraints{fcOneOf = Just s}
        | Set.size s == 1 = Just $ Set.findMin s
        | otherwise = Nothing
      cOneOf _ = Nothing

      -- | If the  bounds on the range are strictly equal then there's only
      --   one valid outcome.
      cRange FloatConstraints{fcGreaterThan=Just (lb,True),fcLessThan=Just (ub,True)}
        | lb == ub = Just lb
        | otherwise = Nothing
      cRange _ = Nothing

  -- | Attempt to lift a concrete value into a constraint that covers it. This
  --   is used to allow us to more easily define the MeetSemiLattice instances
  --   for an Ambiguous t
  makeConstraint :: Float -> Constraints Float
  makeConstraint = is

-- | Map value of FloatConstraints into a subset of that type. The codomain is
--   a portion of the type where structural and value equality are identical,
--   and the mapping preserves the `validate` function.
--
--   TODO :: Add a test to make sure that normalization doesn't change the
--           validation set of the set of constraints.
--
normalize :: FloatConstraints -> FloatConstraints
normalize = nmTop . nmOneOfGT . nmOneOfLT

  where

    -- | Remove all elements from the OneOf set that break the LT bound
    nmOneOfLT :: FloatConstraints -> FloatConstraints
    nmOneOfLT c@FloatConstraints{fcOneOf = Just s,fcLessThan = Just (ub,True)}
      = c {fcOneOf = Just $ Set.filter (<= ub) s}
    nmOneOfLT c@FloatConstraints{fcOneOf = Just s,fcLessThan = Just (ub,False)}
      = c {fcOneOf = Just $ Set.filter (<  ub) s}
    nmOneOfLT c = c

    -- | Remove all elements from the OneOf set that break the GT bound
    nmOneOfGT :: FloatConstraints -> FloatConstraints
    nmOneOfGT c@FloatConstraints{fcOneOf = Just s,fcGreaterThan = Just (lb,True)}
      = c {fcOneOf = Just $ Set.filter (>= lb) s}
    nmOneOfGT c@FloatConstraints{fcOneOf = Just s,fcGreaterThan = Just (lb,False)}
      = c {fcOneOf = Just $ Set.filter (>  lb) s}
    nmOneOfGT c = c

    -- | Check whether the elements are consistent so far, and if not replace
    --   with the unique top element.
    nmTop :: FloatConstraints -> FloatConstraints
    nmTop c
      | fcConsistent c = c
      | otherwise      = top

    -- | Combines all the individual checks to produce a global consistency
    --   check for the integer constraint element.
    fcConsistent :: FloatConstraints -> Bool
    fcConsistent c = all ($ c) $ list [fcEmpty,fcRange]

    -- | If there's no empty one of set, then this is consistent.
    fcEmpty :: FloatConstraints -> Bool
    fcEmpty FloatConstraints{fcOneOf = Just s} = not $ Set.null s
    fcEmpty _ = True

    -- | If the greater than and less than constraints overlap then
    --   this is consistent.
    fcRange :: FloatConstraints -> Bool
    fcRange FloatConstraints{fcGreaterThan=Just (lb,True),fcLessThan=Just (ub,True)}
      = lb <= ub
    fcRange FloatConstraints{fcGreaterThan=Just (lb,_),fcLessThan=Just (ub,_)}
      = lb < ub
    fcRange _ = True

instance Newtype (Constraints Float) FloatConstraints where
  pack = FltCnstr
  unpack (FltCnstr a) = a

instance Eq (Constraints Float) where
  (==) = under2 (\ a b -> normalize a == normalize b)

instance PartialOrd FloatConstraints where
  -- | a `leq` b if each of their internal constraints are induvidually
  --   less than or equal.
  leq a b
      =  leqMaybe leqOneOf (fcOneOf       a') (fcOneOf       b')
      && leqMaybe leqGT    (fcGreaterThan a') (fcGreaterThan b')
      && leqMaybe leqLT    (fcLessThan    a') (fcLessThan    b')

    where

      -- Normalize the two inputs because there's too many edge cases otherwise
      a' = normalize a
      b' = normalize b

      leqOneOf :: Set Float -> Set Float -> Bool
      leqOneOf a b = not $ Set.isProperSubsetOf a b

      leqGT :: (Float,IsInclusive) -> (Float,IsInclusive) -> Bool
      leqGT (a,False) (b,True ) = a <  b
      leqGT (a,_    ) (b,_    ) = a <= b

      leqLT :: (Float,IsInclusive) -> (Float,IsInclusive) -> Bool
      leqLT (a,False) (b,True ) = a >  b
      leqLT (a,_    ) (b,_    ) = a >= b


instance PartialOrd (Constraints Float) where
  leq = under2 leq

instance JoinSemiLattice FloatConstraints where
  (\/) a b = normalize FloatConstraints {
       fcOneOf       = joinMaybe joinOneOf (fcOneOf       a) (fcOneOf       b)
      ,fcGreaterThan = joinMaybe joinGT    (fcGreaterThan a) (fcGreaterThan b)
      ,fcLessThan    = joinMaybe joinLT    (fcLessThan    a) (fcLessThan    b)}

   where

      joinOneOf :: Set Float -> Set Float -> Set Float
      joinOneOf = Set.intersection

      joinGT :: (Float,IsInclusive) -> (Float,IsInclusive) -> (Float,IsInclusive)
      joinGT (a,ai) (b,bi)
        | a == b    = (a,ai && bi)
        | a <  b    = (b,bi)
        | otherwise = (a,ai)

      joinLT :: (Float,IsInclusive) -> (Float,IsInclusive) -> (Float,IsInclusive)
      joinLT (a,ai) (b,bi)
        | a == b    = (a,ai && bi)
        | a >  b    = (b,bi)
        | otherwise = (a,ai)

instance JoinSemiLattice (Constraints Float) where
  (\/) a b = pack $ under2 (\/) a b

instance BoundedJoinSemiLattice FloatConstraints where
  bottom = FloatConstraints Nothing Nothing Nothing

instance BoundedJoinSemiLattice (Constraints Float) where
  bottom = pack $ bottom

-- TODO :: The instance of meet over a set of float constraints. This is
--         used as a generalize operation, to fund supersets where possible.

instance MeetSemiLattice FloatConstraints where
  (/\) a b = undefined

instance MeetSemiLattice (Constraints Float) where
  (/\) a b = pack $ under2 (/\) a b

instance BoundedMeetSemiLattice FloatConstraints where
  top = bottom {fcOneOf = Just Set.empty}

instance BoundedMeetSemiLattice (Constraints Float) where
  top = pack $ top

instance OneOfConstraint Float where
  oneOf f = pack $ bottom {fcOneOf = Just $ Set.fromList f}

instance GTConstraint Float where
  greaterThan   i = pack $ bottom {fcGreaterThan = Just (i,False)}
  greaterThanEq i = pack $ bottom {fcGreaterThan = Just (i,True)}

instance LTConstraint Float where
  lessThan   i = pack $ bottom {fcLessThan = Just (i,False)}
  lessThanEq i = pack $ bottom {fcLessThan = Just (i,True)}

-- | Used along with the above contraint classes to allow for defining an
--   constraints as a list of things. As in the following example.
--
--   > test :: Constraints Float
--   > test = [oneOf [2,3,4], greaterThan 4,lessThanEq 12]
--
instance IsList (Constraints Float) where
  type Item (Constraints Float) = Constraints Float
  fromList = foldr (\/) bottom
  toList t = [t]


-- TODO :: Whenever you get around to it, rewrite the show and read instances
--         so that they use the above list syntax.

deriving instance Show (Constraints Float)
deriving instance Read (Constraints Float)

