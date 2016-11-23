
-- | This defined a class, functions and a set of isntances for an element with
--   a "generality order" this is a lattice ordering where all values will
--   will grow as a design is added to.
module Algebra.GenOrd where

import Algebra.PartialOrd
import Algebra.Lattice

-- | Adds testers to the lattice functions allowing us to collapse multiple
--   possible representations of bottom and top.
class (Eq a,PartialOrd a, BoundedLattice a) => LatticeTest a where
  isBottom :: a -> Bool
  isTop    :: a -> Bool

-- | Class that specifies a generality order, in particular an order where
--   `top` means "failure", `bottom` means "anything/no information/ignorance",
--   `join` means "unify" , and `meet` means "generalize"
class (LatticeTest a) => GenOrd a where

any :: BoundedJoinSemiLattice a => a
any = bottom

fail :: BoundedMeetSemiLattice a => a
fail = top

unify :: JoinSemiLattice a => a -> a -> a
unify = (\/)

generalize :: MeetSemiLattice a => a -> a -> a
generalize = (/\)

-- | Wrapper for a datatype with equality that places a top and bottom on
--   it such that no two elements of the original datatype have an ordering
--   relative to each other.
data ID a = Top | ID a | Bottom
  deriving (Eq, Ord, Show, Read)

instance Eq a => PartialOrd (ID a) where
  leq _ Top = True
  leq Top _ = False
  leq Bottom _ = True
  leq _ Bottom = False
  leq (ID a) (ID b)
    | a == b = True
    | otherwise = False

instance Eq a => JoinSemiLattice (ID a) where
  (\/) Top _ = Top
  (\/) _ Top = Top
  (\/) Bottom b = b
  (\/) a Bottom = a
  (\/) (ID a) (ID b)
    | a == b = ID a
    | otherwise = Top

instance Eq a => MeetSemiLattice (ID a) where
  (/\) Top a = a
  (/\) b Top = b
  (/\) Bottom _ = Bottom
  (/\) _ Bottom = Bottom
  (/\) (ID a) (ID b)
    | a == b = ID a
    | otherwise = Bottom

instance Eq a => BoundedJoinSemiLattice (ID a) where
  bottom = Bottom

instance Eq a => BoundedMeetSemiLattice (ID a) where
  top = Top

instance Eq a => Lattice (ID a)
instance Eq a => BoundedLattice (ID a)

instance Eq a => LatticeTest(ID a) where
  isBottom = (== Bottom)
  isTop = (== Top)

instance Eq a => GenOrd (ID a)
