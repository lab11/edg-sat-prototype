
-- | This module contains phantom types that we use to flag portions of the
--   build process, whether a value is in a form that we can work with, as
--   well as supporting infrastructure
module EDG.PhantomTypes where


-- | Phantom type we use to mark whether an element of a design is ready to
--   be worked with as lattice element or requires additional fixing.
--   For instance a UID specified as `unique` would be unusable, since it
--   requires a context dependent assignment of a unique value before it can
--   become part of a design.
data Usability = Usable | Unusable
  deriving (Show, Read, Eq)


type family GatherUsability a b :: Usability  where
  GatherUsability Usable   Usable   = Usable
  GatherUsability Unusable Usable   = Unusable
  GatherUsability Usable   Unusable = Unusable
  GatherUsability Unusable Unusable = Unusable

