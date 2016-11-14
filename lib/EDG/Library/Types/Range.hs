
module EDG.Library.Types.Range where

import Algebra.PartialOrd
import Algebra.Lattice
import EDG.Algebra.GenOrd

import Data.Maybe (fromMaybe)

-- TODO: Modify to cover inclusive and exclusive ranges

-- | A Range with a partial order for intersection, as well as corresponding
--   meets and joins. Where a < b if b is a subset of a
data Range a = Range {min :: a, max :: a}
  deriving (Eq, Show, Read)

instance PartialOrd a => PartialOrd (Range a) where
  leq (Range min max) (Range min' max') = (min `leq` min') && (min' `leq` max)
                                       && (min `leq` max') && (max' `leq` max)

instance (PartialOrd a,Lattice a) => JoinSemiLattice (Range a) where
  (\/) (Range min max) (Range min' max') = Range (min \/ min') (max /\ max')

instance (PartialOrd a,BoundedLattice a) => BoundedJoinSemiLattice (Range a) where
  bottom = Range bottom top

instance (PartialOrd a,Lattice a) => MeetSemiLattice (Range a) where
  (/\) (Range min max) (Range min' max') = Range (min /\ min') (max \/ max')

instance (PartialOrd a,BoundedLattice a) => BoundedMeetSemiLattice (Range a) where
  top = Range top bottom

instance (PartialOrd a,BoundedLattice a) => Lattice (Range a)
instance (PartialOrd a,BoundedLattice a) => BoundedLattice (Range a)

instance (PartialOrd a,BoundedLattice a) => LatticeTest (Range a) where
  isBottom = (== bottom)
  isTop (Range min max) =  not (min `leq` max)

-- | A renge with a genord defined so that unification is intersection
newtype IntRange a = IntRange (Range a)
  deriving (Eq, Show, Read)

instance Newtype (IntRange a) (Range a) where
  pack = IntRange
  unpack (IntRange a) = a

instance PartialOrd a => PartialOrd (IntRange a) where
  leq (IntRange a) (IntRange b) = a `leq` b

instance (PartialOrd a,Lattice a) => JoinSemiLattice (IntRange a) where
  (\/) (IntRange a) (IntRange b) = IntRange $ a \/ b

instance (PartialOrd a,BoundedLattice a) => BoundedJoinSemiLattice (IntRange a) where
  bottom = IntRange bottom

instance (PartialOrd a,Lattice a) => MeetSemiLattice (IntRange a) where
  (/\) (IntRange a) (IntRange b) = IntRange $ a /\ b

instance (PartialOrd a,BoundedLattice a) => BoundedMeetSemiLattice (IntRange a) where
  top = IntRange top

instance (PartialOrd a,BoundedLattice a) => Lattice (IntRange a)
instance (PartialOrd a,BoundedLattice a) => BoundedLattice (IntRange a)

instance (PartialOrd a,BoundedLattice a) => LatticeTest (IntRange a) where
  isBottom = isBottom . unpack
  isTop = isTop . unpack

instance (PartialOrd a,BoundedLattice a) => GenOrd (IntRange a)

-- | A Range with a genord defined so that unification is union
newtype UniRange a = UniRange (Range a)
  deriving (Eq, Show, Read)

instance Newtype (UniRange a) (Range a) where
  pack = UniRange
  unpack (UniRange a) = a

instance PartialOrd a => PartialOrd (UniRange a) where
  leq (UniRange a) (UniRange b) = b `leq` a

instance (PartialOrd a,Lattice a) => JoinSemiLattice (UniRange a) where
  (\/) (UniRange a) (UniRange b) = UniRange $ a /\ b

instance (PartialOrd a,BoundedLattice a) => BoundedJoinSemiLattice (UniRange a) where
  bottom = UniRange top

instance (PartialOrd a,Lattice a) => MeetSemiLattice (UniRange a) where
  (/\) (UniRange a) (UniRange b) = UniRange $ a \/ b

instance (PartialOrd a,BoundedLattice a) => BoundedMeetSemiLattice (UniRange a) where
  top = UniRange bottom

instance (PartialOrd a,BoundedLattice a) => Lattice (UniRange a)
instance (PartialOrd a,BoundedLattice a) => BoundedLattice (UniRange a)

instance (PartialOrd a,BoundedLattice a) => LatticeTest (UniRange a) where
  isBottom = isTop . unpack
  isTop = isBottom . unpack


