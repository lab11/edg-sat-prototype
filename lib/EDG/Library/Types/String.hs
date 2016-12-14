
module EDG.Library.Types.String where

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

data StringConstraints = StringConstraints {
    scOneOf :: Maybe (Set String)
  , scNoneOf :: Maybe (Set String)
  } deriving (Show, Read, Eq)

instance Constrainable String where
  data Constraints String = StrCnstr StringConstraints

  -- | Given a set of constraints on a value, and a value, return whether the
  --   value matches the set of constraints.
  validate :: Constraints String -> String -> Bool
  validate (StrCnstr StringConstraints{..}) i
    =  all ($ i) . catMaybes $ vList

    where

      -- | The list of all the checks for validity in a form that easy to
      --   evaluate in one fell swoop
      vList :: [Maybe (String -> Bool)]
      vList
        = [vOneOf <$> scOneOf
          ,vNoneOf <$> scNoneOf]

      -- | If there's an oneOf constraint, is it being met?
      vOneOf :: Set String -> String ->  Bool
      vOneOf = flip Set.member

      -- | If there's an NoneOf constraint, is it being met?
      vNoneOf :: Set String -> String ->  Bool
      vNoneOf = flip Set.notMember

  -- | Given a set of constraints, check whether the constraints are realizable
  --   and there might exist a concrete element of that type.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  consistent :: Constraints String -> Bool
  consistent = under' $ (/= top) . normalize

  -- | If a set of constraints can be reduced to a single value return Just
  --   that value. Otherwise return Nothing.
  --
  --   This gets called often as part of the normalization procedure of an
  --   ambiguous value and any implementation should be fast.
  --
  --   The only case we really have to deal with is the instance where there's
  --   a single element in the OneOf set after normalization.
  collapse :: Constraints String -> Maybe String
  collapse = under' $ cOneOf . normalize

    where

      -- | If there's only a single element in the OneOf list after
      --   normalization then that's the value we want to returnEq.
      cOneOf :: StringConstraints -> Maybe String
      cOneOf StringConstraints{scOneOf = Just s}
        | Set.size s == 1 = Just $ Set.findMin s
        | otherwise = Nothing
      cOneOf _ = Nothing

  -- | Attempt to lift a concrete value into a constraint that covers it. This
  --   is used to allow us to more easily define the MeetSemiLattice instances
  --   for an Ambiguous t
  makeConstraint :: String -> Constraints String
  makeConstraint = is

-- | Map value of FloatConstraints into a subset of that type. The codomain is
--   a portion of the type where structural and value equality are identical,
--   and the mapping preserves the `validate` function.
--
--   TODO :: Add a test to make sure that normalization doesn't change the
--           validation set of the set of constraints.
--
normalize :: StringConstraints -> StringConstraints
normalize = nmTop . nmOneOfNone

  where

    -- | Remove all elements from the OneOf set that break the noneOf bound
    --   then remove the NoneOf set, since all the information is already
    --   in the oneOf set.
    nmOneOfNone :: StringConstraints -> StringConstraints
    nmOneOfNone c@StringConstraints{scOneOf = Just s,scNoneOf = Just ns}
      = c {scOneOf = Just $ s `Set.difference` ns,scNoneOf = Nothing}
    nmOneOfNone c = c

    -- | Check whether the elements are consistent so far, and if not replace
    --   with the unique top element.
    nmTop :: StringConstraints -> StringConstraints
    nmTop c
      | scConsistent c = c
      | otherwise      = top

    -- | Combines all the individual checks to produce a global consistency
    --   check for the integer constraint element.
    scConsistent :: StringConstraints -> Bool
    scConsistent = scEmpty

    -- | If there's no empty one of set, then this is consistent.
    scEmpty :: StringConstraints -> Bool
    scEmpty StringConstraints{scOneOf = Just s} = not $ Set.null s
    scEmpty _ = True


instance Newtype (Constraints String) StringConstraints where
  pack = StrCnstr
  unpack (StrCnstr a) = a

instance Eq (Constraints String) where
  (==) = under2 (\ a b -> normalize a == normalize b)

instance PartialOrd StringConstraints where
  -- | a `leq` b if each of their internal constraints are induvidually
  --   less than or equal.
  leq a b
      =  leqMaybe leqOneOf  (scOneOf  a') (scOneOf  b')
      && leqMaybe leqNoneOf (scNoneOf a') (scNoneOf b')

    where

      -- Normalize the two inputs because there's too many edge cases otherwise
      a' = normalize a
      b' = normalize b

      leqOneOf :: Set String -> Set String -> Bool
      leqOneOf a b = not $ Set.isProperSubsetOf a b

      leqNoneOf :: Set String -> Set String -> Bool
      leqNoneOf = Set.isSubsetOf

instance PartialOrd (Constraints String) where
  leq = under2 leq

instance JoinSemiLattice StringConstraints where
  (\/) a b = normalize StringConstraints {
       scOneOf  = joinMaybe joinOneOf  (scOneOf  a) (scOneOf  b)
      ,scNoneOf = joinMaybe joinNoneOf (scNoneOf a) (scNoneOf b)}

   where

      joinOneOf :: Set String -> Set String -> Set String
      joinOneOf = Set.intersection

      joinNoneOf :: Set String -> Set String -> Set String
      joinNoneOf = Set.union

instance JoinSemiLattice (Constraints String) where
  (\/) a b = pack $ under2 (\/) a b

instance BoundedJoinSemiLattice StringConstraints where
  bottom = StringConstraints Nothing Nothing

instance BoundedJoinSemiLattice (Constraints String) where
  bottom = pack $ bottom

-- TODO :: The instance of meet over a set of float constraints. This is
--         used as a generalize operation, to fund supersets where possible.

instance MeetSemiLattice StringConstraints where
  (/\) a b = undefined

instance MeetSemiLattice (Constraints String) where
  (/\) a b = pack $ under2 (/\) a b

instance BoundedMeetSemiLattice StringConstraints where
  top = bottom {scOneOf = Just Set.empty}

instance BoundedMeetSemiLattice (Constraints String) where
  top = pack $ top

instance OneOfConstraint String where
  oneOf f = pack $ bottom {scOneOf = Just $ Set.fromList f}

instance NoneOfConstraint String where
  noneOf f = pack $ bottom {scNoneOf = Just $ Set.fromList f}

-- | Used along with the above contraint classes to allow for defining an
--   constraints as a list of things. As in the following example.
--
--   > test :: Constraints String
--   > test = [oneOf ["SPI","I2C","UART"], isNot "JTAG"]
--
instance IsList (Constraints String) where
  type Item (Constraints String) = Constraints String
  fromList = foldr (\/) bottom
  toList t = [t]

-- TODO :: Whenever you get around to it, rewrite the show and read instances
--         so that they use the above list syntax.

deriving instance Show (Constraints String)
deriving instance Read (Constraints String)

