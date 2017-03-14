
module EDG.Classes.Normalizable where

import Control.Newtype

-- | Normalizeable types have a function that transforms any element of that
--   type into an element that preserves some specific property.
--
--   Use newtypes to specify certain types of normalization.
class Normalizable a where
  -- | Project a value of type `a` into a subspace of that type.
  normalize :: a -> a

-- | Simple version of normalization
norm :: Normalizable a => a -> a
norm = normalize

-- | Convinience for newtype wrappers.
normUnder :: (Normalizable n, Newtype n o) => (o -> n) -> o -> o
normUnder c = under c normalize

-- | Newtype for normalization under predicate equality.
--
--   i.e Any instance of `Normalizeable (PredEq p)` should have the following
--   property.
--
--   prop> asPredicate p == asPredicate (norm p)
--
--   Which says that the predicates decribed by the input and output of `norm`
--   should be indentical.
--
--   In addition, it should make structural equality for the
--   normalized subspace equivalent to predicate equality for the type.
--
--   prop> forall p q. (asPredicate p == asPredicate q) `implies` (norm p == norm q)
--
newtype PredEq a = PredEq a
  deriving (Show, Read, Eq, Ord)

instance Newtype (PredEq a) a where
  pack = PredEq
  unpack (PredEq a) = a
