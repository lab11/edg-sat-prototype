
module EDG.Predicates.Bounds where

import Algebra.Lattice
import Algebra.PartialOrd
import Algebra.AsPredicate

-- Bounds for Integer = (Enum)
-- Bounds for AlgReal = (Num,Fractional,Ord)

type IsInclusive = Bool
pattern Inclusive = True
pattern NonInclusive = False

implies :: Bool -> Bool -> Bool
implies False _ = True
implies True  a = a

-- We want two separate types here since they have very different operations
-- with respect to meets and joins. Done correctly we can then combine them
-- into a more powerful range type that is actually kinda useful.

data LowerBound a = LowerBound IsInclusive a
  deriving (Show, Read)

data UpperBound a = UpperBound IsInclusive a
  deriving (Show, Read)

instance Eq (LowerBound Integer) where
  (==) lb ub = (l == u)
    where
      (LowerBound lInc l) = normalizeEnumLB lb
      (UpperBound uInc u) = normalizeEnumUB ub

instance Eq (LowerBound Float) where
  (==) lb ub = (l == u) && (lb == ub)
    where
      (LowerBound lInc l) = lb
      (UpperBound uInc u) = ub

instance (Ord a) => AsPredicate (LowerBound a) where
  type PredicateDomain (LowerBound a) = a
  asPredicate (LowerBound Inclusive a)    b = b >= a
  asPredicate (LowerBound NonInclusive a) b = b >  a

instance (Ord a) => AsPredicate (UpperBound a) where
  type PredicateDomain (UpperBound a) = a
  asPredicate (UpperBound Inclusive a)    b = b <= a
  asPredicate (UpperBound NonInclusive a) b = b <  a

boundedIsSATLP :: (Eq a,Bounded a) => LowerBound a -> Bool
boundedIsSATLP (LowerBound inc a)
  | a == maxBound = inc
  | otherwise     = True

boundedIsSATUP :: (Eq a,Bounded a) => UpperBound a -> Bool
boundedIsSATUP (UpperBound inc a)
  | a == minBound = inc
  | otherwise     = True


-- We have to declare induvidual isntances of this as needed, it's a pain
instance SATAblePredicate (LowerBound Integer) where
  isSAT _ = True

instance SATAblePredicate (UpperBound Integer) where
  isSAT _ = True

instance SATAblePredicate (LowerBound Float) where
  isSAT _ = True

instance SATAblePredicate (UpperBound Float) where
  isSAT _ = True

instance (Ord a,Bounded a) => CollapseablePredicate (LowerBound a) where
  collapse (LowerBound inc a)
    | (a == maxBound) && inc = Just a
    | otherwise              = Nothing

instance (Ord a,Bounded a) => CollapseablePredicate (UpperBound a) where
  collapse (UpperBound inc a)
    | (a == minBound) && inc = Just a
    | otherwise              = Nothing

instance (Ord a) => PartialOrd (LowerBound a) where
  leq (LowerBound ai a) (LowerBound bi b)
    | a < b = True
    | (a == b) && (bi `implies` ai) = True
    | otherwise = False

instance (Ord a) => PartialOrd (UpperBound a) where
  leq (UpperBound ai a) (UpperBound bi b)
    | a > b = True
    | (a == b) && (bi `implies` ai) = True
    | otherwise = False

instance (Ord a) => JoinSemiLattice (LowerBound a) where
  (\/) (LowerBound ai a) (LowerBound bi b)
    | a == b    = LowerBound (ai && bi) a
    | a <  b    = LowerBound bi b
    | otherwise = LowerBound ai a

instance (Ord a) => JoinSemiLattice (UpperBound a) where
  (\/) (UpperBound ai a) (UpperBound bi b)
    | a == b    = UpperBound (ai && bi) a
    | a <  b    = UpperBound ai a
    | otherwise = UpperBound bi b

instance (Ord a) => MeetSemiLattice (LowerBound a) where
  (/\) (LowerBound ai a) (LowerBound bi b)
    | a == b    = LowerBound (ai || bi) a
    | a <  b    = LowerBound ai a
    | otherwise = LowerBound bi b

instance (Ord a) => MeetSemiLattice (UpperBound a) where
  (/\) (UpperBound ai a) (UpperBound bi b)
    | a == b    = UpperBound (ai || bi) a
    | a <  b    = UpperBound bi b
    | otherwise = UpperBound ai a

-- We can't add any of the other instances here since we don't have ranges or
-- any way to represent bottom/top. Once we add both we can get to some
-- more interesting stuff, and so we'll do it in EDG.Predicates.Maybe and
-- EDG.Predicates.Range.

normalizeEnumLB :: (Enum a) => LowerBound a -> LowerBound a
normalizeEnumLB (LowerBound NonInclusive a) = LowerBound Inclusive (succ a)
normalizeEnumLM a = a

normalizeEnumUB :: (Enum a) => UpperBound a -> UpperBound a
normalizeEnumUB (UpperBound NonInclusive a) = UpperBound Inclusive (pred a)
normalizeEnumUM a = a
