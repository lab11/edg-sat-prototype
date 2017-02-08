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

-- | This is the raw type we have to define, for UIDNs whose partial order is
--   just flat.
newtype UIDN a = UIDN a
  deriving (Show, Read, Eq)

instance Newtype (UIDN a) a where
  pack = UIDN
  unpack (UIDN a) = a

-- | The constraint type for UIDNs, which just treat UIDNs as a partial order
--   where `UIDNTop >= everything` , `UIDNBottom <= everything`, nothing else is
--   similar.
--
--   Also, it is assumed that any `UCNew` is temporary and will be replaced with
--   the appropriate new fixed UIDN from a pool before any operations are run,
--   therefore all ops with `UCNew` will throw runtime errors.
data UIDNCons a
  -- | There is no UIDN that is valid here
  = UCTop
  -- | Every UIDN is valid here
  | UCBottom
  -- | This particular UIDN is valid here
  | UCVal (UIDN a)
  -- | You need to replace this with an actual unique ID before doing any
  --   actual work with this value.
  --
  --   This value causes errors whenever you try to work with it, so make sure
  --   you filter it out of any spec before you build a system out of it.
  | UCNew
  deriving (Show, Read, Eq)

instance Eq a => AsPredicate (UIDNCons a) where
  type PredicateDomain (UIDNCons a) = UIDN a
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

instance Eq a => SATAblePredicate (UIDNCons a) where
  isSAT UCNew =  error "Can't perform ops on a UCNew"
  isSAT UCTop = False
  isSAT _     = True

instance Eq a => CollapseablePredicate (UIDNCons a) where
  collapse UCNew     = error "Can't perform ops on a UCNew"
  collapse (UCVal c) = Just c
  collapse _         = Nothing

instance Eq a => LiftablePredicate (UIDNCons a) where
  liftPredicate = UCVal

instance Eq a => BottomPredicate (UIDNCons a) where
  isBottom UCBottom = True
  isBottom _ = False

instance Eq a => PartialOrd (UIDNCons a) where
  leq UCNew _ = error "Can't perform ops on a UCNew"
  leq _ UCNew = error "Can't perform ops on a UCNew"
  leq _         UCTop     = True
  leq UCTop     _         = False
  leq UCBottom  _         = True
  leq _         UCBottom  = False
  leq (UCVal a) (UCVal b) = a == b

instance Eq a => JoinSemiLattice (UIDNCons a) where
  (\/) UCNew _ = error "Can't perform ops on a UCNew"
  (\/) _ UCNew = error "Can't perform ops on a UCNew"
  (\/) UCTop _ = UCTop
  (\/) _ UCTop = UCTop
  (\/) UCBottom a = a
  (\/) a UCBottom = a
  (\/) (UCVal a) (UCVal b)
    | a == b    = UCVal a
    | otherwise = UCTop

instance Eq a => MeetSemiLattice (UIDNCons a) where
  (/\) UCNew _ = error "Can't perform ops on a UCNew"
  (/\) _ UCNew = error "Can't perform ops on a UCNew"
  (/\) UCTop a = a
  (/\) a UCTop = a
  (/\) UCBottom _ = UCBottom
  (/\) _ UCBottom = UCBottom
  (/\) (UCVal a) (UCVal b)
    | a == b = UCVal a
    | otherwise = UCBottom

instance Eq a => BoundedJoinSemiLattice (UIDNCons a) where
  bottom = UCBottom

instance Eq a => BoundedMeetSemiLattice (UIDNCons a) where
  top = UCTop

instance Eq a => Constrainable (UIDN a) where
  type Constraints (UIDN a) = UIDNCons a

-- We don't define an IsList constraint here, since there's no valid
-- constructor for a UIDN in a definition other than `top`, `bottom` and `unique`
-- nobody should be specifying actual values of a UIDN.

-- | Ask for a unique value to be used as this UIDN upon insertion into the
--   system.
unique :: UIDNCons a
unique = UCNew

-- | An alias for the UID type we use all over the place.
type UID = UIDN Integer
type UIDCons = UIDNCons Integer
