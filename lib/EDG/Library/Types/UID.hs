-- UIDs are integers when reified, but otherwise are either "any" or new UIDs
-- to be generated as they're needed in the system. Conversion happens on
-- transformation not insertion
--
-- TODO :: Make sure you're hiding the constructors that users aren't allowed
-- to use. It should at least be a basic check on them doing silly shit.
module EDG.Library.Types.UID where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate
import Algebra.Constrainable

import Control.Newtype
import Control.Newtype.Util

import GHC.Exts

-- | This is the raw type we have to define, for UIDNewtypes whose partial order is
--   just flat.
newtype UIDNewtype a = UIDNewtype a
  deriving (Show, Read, Eq)

instance Newtype (UIDNewtype a) a where
  pack = UIDNewtype
  unpack (UIDNewtype a) = a

-- | The constraint type for UIDNewtypes, which just treat UIDNewtypes as a partial order
--   where `UIDNewtypeTop >= everything` , `UIDNewtypeBottom <= everything`, nothing else is
--   similar.
--
--   Also, it is assumed that any `UCNew` is temporary and will be replaced with
--   the appropriate new fixed UIDNewtype from a pool before any operations are run,
--   therefore all ops with `UCNew` will throw runtime errors.
data UIDNewtypeCons a
  -- | There is no UIDNewtype that is valid here
  = UCTop
  -- | Every UIDNewtype is valid here
  | UCBottom
  -- | This particular UIDNewtype is valid here
  | UCVal (UIDNewtype a)
  -- | You need to replace this with an actual unique ID before doing any
  --   actual work with this value.
  --
  --   This value causes errors whenever you try to work with it, so make sure
  --   you filter it out of any spec before you build a system out of it.
  | UCNew
  deriving (Show, Read, Eq)

instance Eq a => AsPredicate (UIDNewtypeCons a) where
  type PredicateDomain (UIDNewtypeCons a) = UIDNewtype a
  asPredicate UCTop     = const False
  asPredicate UCBottom  = const True
  asPredicate (UCVal v) = (== v)
  -- I'm generally not a fan of having partial functions anywhere in this
  -- sort of hierarchy, but I can't think of any way to do this except with
  -- phantom types, and that gets really kludgy really fast.
  --
  -- TODO :: Find a non-partial way to spli the type here between different
  --         domains.
  asPredicate UCNew     = error "Can't perform ops on a UCNew"

instance Eq a => SATAblePredicate (UIDNewtypeCons a) where
  isSAT UCNew = True
  isSAT UCTop = False
  isSAT _     = True

instance Eq a => CollapseablePredicate (UIDNewtypeCons a) where
  collapse UCNew     = error "Can't perform ops on a UCNew"
  collapse (UCVal c) = Just c
  collapse _         = Nothing

instance Eq a => LiftablePredicate (UIDNewtypeCons a) where
  liftPredicate = UCVal

instance Eq a => BottomPredicate (UIDNewtypeCons a) where
  isBottom UCBottom = True
  isBottom _ = False

instance Eq a => PartialOrd (UIDNewtypeCons a) where
  leq UCNew _ = error "Can't perform ops on a UCNew"
  leq _ UCNew = error "Can't perform ops on a UCNew"
  leq _         UCTop     = True
  leq UCTop     _         = False
  leq UCBottom  _         = True
  leq _         UCBottom  = False
  leq (UCVal a) (UCVal b) = a == b

instance Eq a => JoinSemiLattice (UIDNewtypeCons a) where
  (\/) UCNew _ = error "Can't perform ops on a UCNew"
  (\/) _ UCNew = error "Can't perform ops on a UCNew"
  (\/) UCTop _ = UCTop
  (\/) _ UCTop = UCTop
  (\/) UCBottom a = a
  (\/) a UCBottom = a
  (\/) (UCVal a) (UCVal b)
    | a == b    = UCVal a
    | otherwise = UCTop

instance Eq a => MeetSemiLattice (UIDNewtypeCons a) where
  (/\) UCNew _ = error "Can't perform ops on a UCNew"
  (/\) _ UCNew = error "Can't perform ops on a UCNew"
  (/\) UCTop a = a
  (/\) a UCTop = a
  (/\) UCBottom _ = UCBottom
  (/\) _ UCBottom = UCBottom
  (/\) (UCVal a) (UCVal b)
    | a == b = UCVal a
    | otherwise = UCBottom

instance Eq a => BoundedJoinSemiLattice (UIDNewtypeCons a) where
  bottom = UCBottom

instance Eq a => BoundedMeetSemiLattice (UIDNewtypeCons a) where
  top = UCTop

instance Eq a => Constrainable (UIDNewtype a) where
  type Constraints (UIDNewtype a) = UIDNewtypeCons a

-- We don't define an IsList constraint here, since there's no valid
-- constructor for a UIDNewtype in a definition other than `top`, `bottom` and `unique`
-- nobody should be specifying actual values of a UIDNewtype.

-- | Ask for a unique value to be used as this UIDNewtype upon insertion into the
--   system.
unique :: UIDNewtypeCons a
unique = UCNew

-- | An alias for the UID type we use all over the place.
type UID' = UIDNewtype Integer

pattern UID' :: Integer -> UID'
pattern UID' a = UIDNewtype a

type UIDCons = UIDNewtypeCons Integer


