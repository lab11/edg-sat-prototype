-- UIDs are integers when reified, but otherwise are either "any" or new UIDs
-- to be generated as they're needed in the system. Conversion happens on
-- transformation not insertion
-- .
module EDG.Library.Types.UID where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate
import Algebra.Constrainable

import Control.Newtype
import Control.Newtype.Util

import GHC.Exts

-- | This is the raw type we have to define, for UIDs whose partial order is
--   just flat.
newtype UID a = UID a
  deriving (Show, Read, Eq)

instance Newtype (UID a) a where
  pack = UID
  unpack (UID a) = a

-- | The constraint type for UIDs, which just treat UIDs as a partial order
--   where `UIDTop >= everything` , `UIDBottom <= everything`, nothing else is
--   similar.
--
--   Also, it is assumed that any `UCNew` is temporary and will be replaced with
--   the appropriate new fixed UID from a pool before any operations are run,
--   therefore all ops with `UCNew` will throw runtime errors.
data UIDCons a
  -- | There is no UID that is valid here
  = UCTop
  -- | Every UID is valid here
  | UCBottom
  -- | This particular UID is valid here
  | UCVal (UID a)
  -- | You need to replace this with an actual unique ID before doing any
  --   actual work with this value.
  --
  --   This value causes errors whenever you try to work with it, so make sure
  --   you filter it out of any spec before you build a system out of it.
  | UCNew
  deriving (Show, Read, Eq)

instance Eq a => AsPredicate (UIDCons a) where
  type PredicateDomain (UIDCons a) = UID a
  asPredicate UCTop     = const False
  asPredicate UCBottom  = const True
  asPredicate (UCVal v) = (== v)
  asPredicate UCNew     = error "Can't perform ops on a UCNew"

instance Eq a => SATAblePredicate (UIDCons a) where
  isSAT UCNew =  error "Can't perform ops on a UCNew"
  isSAT UCTop = False
  isSAT _     = True

instance Eq a => CollapseablePredicate (UIDCons a) where
  collapse UCNew     = error "Can't perform ops on a UCNew"
  collapse (UCVal c) = Just c
  collapse _         = Nothing

instance Eq a => LiftablePredicate (UIDCons a) where
  liftPredicate = UCVal

instance Eq a => PartialOrd (UIDCons a) where
  leq UCNew _ = error "Can't perform ops on a UCNew"
  leq _ UCNew = error "Can't perform ops on a UCNew"
  leq _         UCTop     = True
  leq UCTop     _         = False
  leq UCBottom  _         = True
  leq _         UCBottom  = False
  leq (UCVal a) (UCVal b) = a == b

instance Eq a => JoinSemiLattice (UIDCons a) where
  (\/) UCNew _ = error "Can't perform ops on a UCNew"
  (\/) _ UCNew = error "Can't perform ops on a UCNew"
  (\/) UCTop _ = UCTop
  (\/) _ UCTop = UCTop
  (\/) UCBottom a = a
  (\/) a UCBottom = a
  (\/) (UCVal a) (UCVal b)
    | a == b    = UCVal a
    | otherwise = UCTop

instance Eq a => MeetSemiLattice (UIDCons a) where
  (/\) UCNew _ = error "Can't perform ops on a UCNew"
  (/\) _ UCNew = error "Can't perform ops on a UCNew"
  (/\) UCTop a = a
  (/\) a UCTop = a
  (/\) UCBottom _ = UCBottom
  (/\) _ UCBottom = UCBottom
  (/\) (UCVal a) (UCVal b)
    | a == b = UCVal a
    | otherwise = UCBottom

instance Eq a => BoundedJoinSemiLattice (UIDCons a) where
  bottom = UCBottom

instance Eq a => BoundedMeetSemiLattice (UIDCons a) where
  top = UCTop

instance Eq a => Constrainable (UID a) where
  type Constraints (UID a) = UIDCons a

-- We don't define an IsList constraint here, since there's no valid
-- constructor for a UID in a definition other than `top`, `bottom` and `unique`
-- nobody should be specifying actual values of a UID.

-- | Ask for a unique value to be used as this UID upon insertion into the
--   system.
unique :: UIDCons a
unique = UCNew

