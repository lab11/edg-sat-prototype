
-- | This defined a class, functions and a set of isntances for an element with
--   a "generality order" this is a lattice ordering where all values will
--   will grow as a design is added to.
module EDG.Algebra.GenOrd where

import Algebra.Lattice

class (Eq a, BoundedLattice a) => GenOrd a
