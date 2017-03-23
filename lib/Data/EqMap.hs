
module Data.EqMap where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Maybe

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

-- | An EqMap is a map where you can specify that the keys for a pair of
--   values are actually equal, at which point the map will use a join to
--   combine the pair of values.
--
--   This is great for something like type unification, you just walk through
--   your system finding types, and when you see them connected you tell the
--   map that they're equal.
--
--   This makes sure all the relevant equality classes are collected nicely
--   together, and are completely transitive.
--
--   TODO :: I'm pretty sure none of these need a bimap, unless I'm searching
--           on the value terms or equality classes as a whole. See if you can
--           remove them when the full interface for EqMap is settled.
--
data EqMap k v = EqMap {
    toEq  :: Map k EqClass
  , toKey :: Bimap EqClass (Set k)
  , toVal :: Bimap EqClass v
  }

-- * Operators

-- Extract Element from eqmap, returns bottom if the value isn't in the
-- map, as it is assumed all unconnected values are simply bottom.
(!) :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> k -> v
(!) m k = fromMaybe bottom (lookup k m)

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
numVals EqMap{..} = Bimap.size toKey

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

-- | Look up an element but return a default if it's not in the map.
findWithDefault :: (Ord k, Ord v, BoundedJoinSemiLattice v) => v -> k -> EqMap k v -> v
findWithDefault d k em = fromMaybe d (lookup k em)

-- * Construction

-- | the empty eqmap
empty :: (Ord k, Ord v, BoundedJoinSemiLattice v) =>  EqMap k v
empty = EqMap {toEq = Map.empty, toKey = Bimap.empty, toVal = Bimap.empty}

-- | the singleton eqmap
singleton :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> v -> EqMap k v
singleton k v = insert k v empty

-- * Insertion/SetEq

-- | Insert the element into map. If it's not already a member it gets its
--   own equality class, if it is then the value is joined with an already
--   existing value.
insert :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> v -> EqMap k v -> EqMap k v
insert k v em@EqMap{..} = case Map.lookup k toEq of
  Just id -> em{ toVal = Bimap.adjust (\/ v) id toVal }
  Nothing -> EqMap { toEq  = Map.insert k newId toEq
                   , toKey = Bimap.insert newId (Set.singleton k) toKey
                   , toVal = Bimap.insert newId v toVal
                   }
    where
      newId = getNextClass em

-- | Set two keys are reffering to the same element.
--
-- insert both elements as bottom
-- getIDa & IDb
--   toEq .= all elems of IDb set to IDa
--   toKey .= delete IDb . (IDa += IDb)
--   toVal .= delete IDb . (IDa = IDa \/ IDb)
--
--   TODO :: There's no reason this should be non-total, but I'm not sure how
--           construct it so that we don't even use `error` without changing
--           the type signature.
--
setEq :: (Ord k, Ord v, BoundedJoinSemiLattice v) => k -> k -> EqMap k v -> EqMap k v
setEq k k' em
  | k == k'   = insert k bottom em
  | otherwise = set . (insert k bottom) . (insert k' bottom) $ em
  where
    set em@EqMap{..} = fromMaybe (error "This should never fail") $ do
      id   <-  Map.lookup k  toEq
      id'  <-  Map.lookup k' toEq
      set' <-  Bimap.lookup id' toKey
      val' <-  Bimap.lookup id' toVal
      return $ EqMap {
          toEq   = Map.map (replace id' id) toEq
        , toKey  = Bimap.delete id' . Bimap.adjust (Set.union set') id $ toKey
        , toVal  = Bimap.delete id' . Bimap.adjust (\/ val')        id $ toVal
        }

    replace a b k
      | k == a    = b
      | otherwise = k

-- * Combine

-- TODO :: These functions if neccesary.
--
-- union :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> EqMap k v -> EqMap k v
-- union = undefined
--
-- unions :: (Ord k, Ord v, BoundedJoinSemiLattice v) => [EqMap k v] -> EqMap k v
-- unions = foldr union empty

-- * Traversal

-- TODO :: Just write instances for Functor, Foldable, and Traversable the
--         rest will be easy to derive from there.

-- map :: (v -> v') -> EqMap k v -> EqMap k v'
-- map = undefined
--
-- mapWithKeys :: (Set k -> v -> v') -> EqMap k v -> EqMap k v'
-- mapWithKeys = undefined

-- * Conversion

-- | Get all the values in the in the EqMap, this won't duplicate equality
--   classes in the resulting list.
elems :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [v]
elems EqMap{..} = Bimap.keysR toVal

-- | Get a list of keys in the EqMap
keys :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [k]
keys EqMap{..} = Map.keys toEq

-- | Gets a list of equality classes and make a list out of them,
--
--   TODO :: This should be the base of the EqMap's IsList instance when you
--           get around to writing that up.
--
classes :: (Ord k, Ord v, BoundedJoinSemiLattice v) => EqMap k v -> [(Set k,v)]
classes EqMap{..} = map (\ id -> (toKey Bimap.! id, toVal Bimap.! id)) $ Bimap.keys toKey

-- | Generates an equality map from a set of equality classes, so that the
--
--   TODO :: This should also be part of the relevant IsList instance when you
--           write that.
--
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

-- | Given an EqMap, find an unused key. I make no guarentees about the space
--   of keys being filled in, just that this gets you an as yet unused key.
getNextClass :: (Ord k, Ord v, JoinSemiLattice v) =>  EqMap k v -> EqClass
getNextClass EqMap{..} =  (+ 1) . fst $ Bimap.findMax toKey
