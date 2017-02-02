
module Data.EqMap where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Control.Newtype
import Control.Newtype.Util

import Algebra.PartialOrd
import Algebra.Lattice

import Prelude hiding (lookup)

newtype EqClass = EqClass Integer
  deriving (Eq, Ord, Show, Read)

instance Newtype EqClass Integer where
  pack = EqClass
  unpack (EqClass i) = i

-- Autogenerating this instance breaks it ... so I'll just do it manually.
instance Num EqClass where
 (+) = under2' (+)
 (-) = under2' (-)
 (*) = under2' (*)
 negate = over EqClass negate
 abs = over EqClass abs
 signum = over EqClass signum
 fromInteger = EqClass . fromInteger

data EqMap k v = EqMap {
    toEq     :: Map k EqClass
  , fromEq   :: Bimap EqClass (Set k)
  , toVal    :: Bimap EqClass v
  }

-- instance (Ord k,Ord v) => Functor (EqMap k) where

-- instance (Ord k) => Foldable (EqMap k) where
--   foldMap f em@EqMap{..} = foldMap f toVal
--
-- instance (Ord k) => Traversable (EqMap k) where
--   traverse :: Applicative f => (a -> f b) -> EqMap k a -> f (EqMap k b)
--   traverse f em@EqMap{..} = (\ v -> em{toVal = v}) <$> traverse f toVal

-- instance (Ord k, Eq v) => Eq (EqMap k v) where

-- instance (Ord k) => IsList (EqMap k) where

-- instance Show (EqMap k v) where

-- instance Read (EqMap k v) where

-- instance Monoid (EqMap k v) where

-- instance Semigroup (EqMap k v) where

-- instance JoinSemiLattice (EqMap k v) where

-- instance BoundedJoinSemiLattice (EqMap k v) where

-- instance PartialOrd (EqMap k v) where

-- * Operators

-- Extract Element from eqmap, calls error is value not in map
(!) :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> k -> v
(!) m k = let Just v = lookup k m in v

-- * Query

-- | Is EqMap Empty?
null :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> Bool
null EqMap{..} = Map.null toEq

-- | Number of Keys in EqMap
size :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> Int
size = numKeys

-- | Number of Keys in EqMap
numKeys :: (Ord k, Ord v, BoundedJoinSemiLattice v) =>  EqMap k v -> Int
numKeys EqMap{..} = Map.size toEq

-- | Number of values/equality classes
numVals :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> Int
numVals EqMap{..} = Bimap.size fromEq

-- | Is Key in the EqMap?
member :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> EqMap k v -> Bool
member k EqMap{..} = Map.member k toEq

-- | Is Key not in EqMap?
notMember :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> EqMap k v -> Bool
notMember k = not . member k

-- | Looks up whether a value is in the EqMap
lookup :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> EqMap k v -> Maybe v
lookup k EqMap{..} = do
  id <- Map.lookup k toEq
  Bimap.lookup id toVal

-- | Look up an element but return a default if it's not in the map
findWithDefault :: (Ord k, Ord v, BoundedJoinSemiLattice v) => v -> k -> EqMap k v -> v
findWithDefault = undefined

-- * Construction

-- | the empty eqmap
empty :: (Ord k, Ord v, BoundedJoinSemiLattice v) =>  EqMap k v
empty = EqMap {toEq = Map.empty, fromEq = Bimap.empty, toVal = Bimap.empty}

-- | the singleton eqmap
singleton :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> v -> EqMap k v
singleton k v = insert k v empty

-- * Insertion/SetEq

-- | Insert the element into map. If it's not already a member it gets its
--   own equality class, if it is then the value is joined with an already
--   existing value.
insert :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> v -> EqMap k v -> EqMap k v
insert k v em@EqMap{..} = case Map.lookup k toEq of
  Just id -> em{ toEq  = Map.insert k id toEq
               , toVal = Bimap.adjust (\/ v) id toVal
               }
  Nothing -> EqMap { toEq   = Map.insert k newId toEq
                   , fromEq = Bimap.insert newId (Set.singleton k) fromEq
                   , toVal  = Bimap.insert newId v toVal
                   }
    where
      newId = getNextClass em

-- | Set two keys are reffering to the same element.
--
-- insert both elements as bottom
-- getIDa & IDb
--   toEq .= all elems of IDb set to IDa
--   fromEq .= delete IDb . (IDa += IDb)
--   toVal .= delete IDb . (IDa = IDa \/ IDb)
setEq :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> k -> EqMap k v -> EqMap k v
setEq k k' em
  | k == k' = insert k bottom em
  | otherwise = set . (insert k bottom) . (insert k' bottom) $ em
  where
    set em@EqMap{..} =
      let Just id   = Map.lookup k  toEq in
      let Just id'  = Map.lookup k' toEq in
      let Just set' = Bimap.lookup id' fromEq in
      let Just val' = Bimap.lookup id' toVal in
      EqMap { toEq   = Map.map (replace id' id) toEq
            , fromEq = Bimap.delete id' . Bimap.adjust (Set.union set') id $ fromEq
            , toVal  = Bimap.delete id' . Bimap.adjust (\/ val') id $ toVal
            }

    replace a b k
      | k == a    = b
      | otherwise = k

-- * Combine

union :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> EqMap k v -> EqMap k v
union = undefined

unions :: (Ord k, Ord v, BoundedJoinSemiLattice v) => [EqMap k v] -> EqMap k v
unions = foldr union empty

-- * Traversal

-- map :: (v -> v') -> EqMap k v -> EqMap k v'
-- map = undefined
--
-- mapWithKeys :: (Set k -> v -> v') -> EqMap k v -> EqMap k v'
-- mapWithKeys = undefined

-- * Conversion

elems :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [v]
elems EqMap{..} = Bimap.keysR toVal

keys :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [k]
keys EqMap{..} = Map.keys toEq

classes :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [(Set k,v)]
classes EqMap{..} = map (\ id -> (fromEq Bimap.! id, toVal Bimap.! id)) $ Bimap.keys fromEq

fromClasses :: (Ord k, Ord v, BoundedJoinSemiLattice v) => [(Set k,v)] -> EqMap k v
fromClasses = foldr insertClass empty
  where
    -- TODO :: This is profoundly inefficient, fix it later.
    insertClass :: (Ord k, Ord v, BoundedJoinSemiLattice v) => (Set k,v) -> EqMap k v -> EqMap k v
    insertClass (sk,v)
      | Set.null sk = id
      | otherwise = let base = Set.findMin sk in
          (\ m -> foldr (setEq base) m sk) . insert base v

-- Internal Functions

getNextClass :: (Ord k, Ord v, JoinSemiLattice v) =>  EqMap k v -> EqClass
getNextClass EqMap{..} =  (+ 1) . fst $ Bimap.findMax fromEq
