
module EDG.EDGInstances.Record where

import Data.EqMap (EqMap)
import qualified Data.EqMap as EqMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Void

import Control.Newtype

import Control.Monad (foldM)
import Control.Monad.Ether.Implicit
import qualified Control.Monad.Ether.State.Class
import Control.Monad.MonadSymbolic
import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
  )
import Data.SBV.Internals (
  CWVal(..)
  )
import qualified Data.SBV as S
import Control.Monad.Scribe
import Control.Monad.Identity (Identity)

import Control.Lens.Ether.Implicit
import Control.Lens.TH

import Control.Newtype.Util

import Algebra.PartialOrd
import Algebra.Lattice

import Algebra.Constrainable
import Algebra.AsPredicate

import EDG.Library.Types
import EDG.Predicates
import EDG.EDGMonad hiding (trace)
import EDG.EDGInstances.Bool
import EDG.SBVWrap
import EDG.EDGDatatype

import EDG.EDGInstances.Bool
import EDG.EDGInstances.String
import EDG.EDGInstances.Float
import EDG.EDGInstances.UID
import EDG.EDGInstances.Integer

import Text.Pretty.Simple
import Debug.Trace

-- TODO :: Get better standard form for error checking. Probably using a
--         MaybeT somehow.
-- TODO :: Consider using MonadTransControl to allow for transations that
--         abort and restart when errors occour. This might allow for multiple
--         error messages and stuff.
--         Look at the package transformers-abort and monad-abort-fd too to see
--         if they can help.

-- | No other good place to keep this instance for now.
--
--   TODO :: Find better location for this instance
instance MonadConstrain EDGMonad (Ref Bool) where
  constrain s = returnAnd () $ errContext context $ sbv s >>= constrain
    where
      context = "constrain `" ++ show s ++ "`"

-- | No other good place to keep this instance for now.
--
--   TODO :: Find better location for this instance
instance MonadConstrain EDGMonad (Ref Value) where
  constrain s = errContext context $ do
    assertValKind s (Bool ())
    returnAnd () $ errContext context $ do
      vs <- sbv s
      case vsValSBV vs of
        Bool bs -> constrain bs
        _ -> throw $ "Tried to constrain problem with value `" ++ show vs
                     ++ "` but it doesn't have kind Bool."
    where
      context = "constrain `" ++ show s ++ "`"

instance S.EqSymbolic ValueSBV where

  (.==) a@ValueSBV{vsKindSBV=ak,vsValSBV=av}
        b@ValueSBV{vsKindSBV=bk,vsValSBV=bv}
    | Int    as <- av, Int    bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | Bool   as <- av, Bool   bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | Float  as <- av, Float  bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | String as <- av, String bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | UID    as <- av, UID    bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | Record as <- av, Record bs <- bv = (ak S..== bk) S.&&& (as S..== bs)
    | otherwise = S.literal False

  (./=) a@ValueSBV{vsKindSBV=ak,vsValSBV=av}
        b@ValueSBV{vsKindSBV=bk,vsValSBV=bv}
    | Int    as <- av, Int    bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | Bool   as <- av, Bool   bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | Float  as <- av, Float  bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | String as <- av, String bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | UID    as <- av, UID    bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | Record as <- av, Record bs <- bv = (ak S../= bk) S.||| (as S../= bs)
    | otherwise = S.literal True

vsLT :: ValueSBV -> ValueSBV -> SBV Bool
vsLT a@ValueSBV{vsKindSBV=ak,vsValSBV=av,vsRefName=an}
     b@ValueSBV{vsKindSBV=bk,vsValSBV=bv,vsRefName=bn}
  | Int   as <- av, Int   bs <- bv = (as S..< bs)
  | Float as <- av, Float bs <- bv = (as S..< bs)
  | otherwise = error $ "Attempting to compare (.<) `" ++ show a ++ "` and `"
      ++ show b ++ "` which have invalid kinds for this operation."

vsLTE :: ValueSBV -> ValueSBV -> SBV Bool
vsLTE a@ValueSBV{vsKindSBV=ak,vsValSBV=av,vsRefName=an}
      b@ValueSBV{vsKindSBV=bk,vsValSBV=bv,vsRefName=bn}
  | Int   as <- av, Int   bs <- bv = (as S..<= bs)
  | Float as <- av, Float bs <- bv = (as S..<= bs)
  | otherwise = error $ "Attempting to compare (.<=) `" ++ show a ++ "` and `"
      ++ show b ++ "` which have invalid kinds for this operation."

vsGT :: ValueSBV -> ValueSBV -> SBV Bool
vsGT a@ValueSBV{vsKindSBV=ak,vsValSBV=av,vsRefName=an}
     b@ValueSBV{vsKindSBV=bk,vsValSBV=bv,vsRefName=bn}
  | Int   as <- av, Int   bs <- bv = (as S..> bs)
  | Float as <- av, Float bs <- bv = (as S..> bs)
  | otherwise = error $ "Attempting to compare (.>) `" ++ show a ++ "` and `"
      ++ show b ++ "` which have invalid kinds for this operation."

vsGTE :: ValueSBV -> ValueSBV -> SBV Bool
vsGTE a@ValueSBV{vsKindSBV=ak,vsValSBV=av,vsRefName=an}
      b@ValueSBV{vsKindSBV=bk,vsValSBV=bv,vsRefName=bn}
  | Int   as <- av, Int   bs <- bv = (as S..>= bs)
  | Float as <- av, Float bs <- bv = (as S..>= bs)
  | otherwise = error $ "Attempting to compare (.>=) `" ++ show a ++ "` and `"
      ++ show b ++ "` which have invalid kinds for this operation."


-- | Turn a value reference into the symbolic data variable.
valRefSBV :: ValRef -> SBVMonad ValSBV
valRefSBV vr
  | Int    i <- vr = errContext context $ Int    <$> sbv i
  | Bool   b <- vr = errContext context $ Bool   <$> sbv b
  | Float  f <- vr = errContext context $ Float  <$> sbv f
  | String s <- vr = errContext context $ String <$> sbv s
  | UID    u <- vr = errContext context $ UID    <$> sbv u
  | Record r <- vr = errContext context $ Record <$> sbv r
  | KVBot  e <- vr = errContext context $ throw $ "ValRef with EqClass `" ++ show e ++ "` is still"
    ++ "ambiguous, cannot generate concrete instantiation."
  | KVTop v <- vr  = absurd v
  where context = "valRevSBV `" ++ show vr ++ "`"

-- | Given a valueSBV, get the kind of that value
getValueKind :: Ref Value -> SBVMonad ValKind
getValueKind v = errContext context $ do
  mvi <- uses @SBVS valInfo (Map.lookup v)
  case mvi of
    Just ValInfo{..} -> valRefKind viValRef
    Nothing -> throw $ "Could not find valInfo for `" ++ show v ++ "`"
  where
    context = "getting kind for value with reference `" ++ show v ++ "`"

-- | Get the kind of a value reference
valRefKind :: ValRef -> SBVMonad ValKind
valRefKind (Int    _) = return $ Int ()
valRefKind (Bool   _) = return $ Bool ()
valRefKind (Float  _) = return $ Float ()
valRefKind (String _) = return $ String ()
valRefKind (UID    _) = return $ UID ()
valRefKind (Record r) = errContext context $ do
  ri <- getRecInfoSBV r
  let req = ri ^. eqClass
  return $ Record req
  where
    context = "getting the kind of a valueRef to `" ++ show r ++ "`"
valRefKind (KVBot _) = return $ KVBot ()
valRefKind (KVTop v) = absurd v

-- | Get the information for a record.
getRecInfoSBV :: Ref Record -> SBVMonad RecInfo
getRecInfoSBV r = errContext context $ do
  -- Get the record info
  mri <- uses @SBVS recInfo (Map.lookup r)
  case mri of
    Just ri -> return ri
    Nothing -> throw $ "No recordInfo for record `" ++ show r ++ "`"
  where
    context = "getRecInfo `" ++ show r ++ "`"


-- | Get the kind from a valinfo
valInfoKind :: ValInfo -> SBVMonad ValKind
valInfoKind ValInfo{..} = valRefKind viValRef

-- | Get the kind for a static value.
valueKind :: Value -> EDGMonad ValKind
valueKind (unpack -> v)
 | (Int    _) <- v = return $ Int ()
 | (Bool   _) <- v = return $ Bool ()
 | (Float  _) <- v = return $ Float ()
 | (String _) <- v = return $ String ()
 | (UID    _) <- v = return $ UID ()
 | (Record r) <- v = errContext context $ do
    ec <- addRecordKind =<< recordKind r
    return . Record $ ec
 | (KVBot  v) <- v = absurd v
 | (KVTop  v) <- v = absurd v
  where
    context = "calculating kind for `" ++ show v ++ "`"

-- | get the kind for a constrained value
constrainedKind :: Constrained -> EDGMonad ValKind
constrainedKind (unpack -> c)
  | unSAT c = throw $ "Contraints are unsatisfiable for `" ++ show c
     ++ "` cannot get a kind."
  | (Int    _) <- c = errContext context $ return $ Int ()
  | (Bool   _) <- c = errContext context $ return $ Bool ()
  | (Float  _) <- c = errContext context $ return $ Float ()
  | (String _) <- c = errContext context $ return $ String ()
  | (UID    _) <- c = errContext context $ return $ UID ()
  | (Record r) <- c = errContext context $ do
     ec <- addRecordKind =<< recConsKind r
     return . Record $ ec
  | (KVBot  _) <- c = errContext context $ return $ KVBot ()
  | (KVTop  _) <- c = throw $ "Contraints are unsatisfiable for `" ++ show c
     ++ "` cannot get a kind."
  where
    context = "calculating kind for `" ++ show c ++ "`"

-- | gets the kind for an ambiguous value
ambigValKind :: Ambiguous Value -> EDGMonad ValKind
ambigValKind Impossible   = throw @String "Cannot find kind for unsatisfiable value"
ambigValKind (Abstract c) = constrainedKind c
ambigValKind (Concrete v) = valueKind v

-- | Adds a kind to the tracked set of kinds
addRecordKind :: RecKind -> EDGMonad RecEqClass
addRecordKind k = errContext context $ do
  eqc <- newRecEqClass
  recordKinds @GS %= Map.insert eqc k
  errContext (context ++ " `" ++ show eqc ++ "`") $ return eqc
  where
    context = "addRecordKind `" ++ show k ++ "`"

-- | gets the kind for a record
recordKind :: Record -> EDGMonad RecKind
recordKind (unpack -> fm) = errContext context $ mapM valueKind fm
  where
    context = "calculating kind for `" ++ show fm ++ "`"

-- | gets the kind for a recordCons
recConsKind :: RecCons -> EDGMonad RecKind
recConsKind RCTop = throw @String "cannot calculate kind for unsatisfiable record"
recConsKind RCBottom = return Map.empty
recConsKind ra@RCAmbig{..} = errContext context $ mapM ambigValKind rcMap
  where
    context = "calculating kind for `" ++ show ra ++ "`"

-- | Ensure that a value has a given kind
assertValKind :: Ref Value -> ValKind -> EDGMonad ()
assertValKind = assertValKind' 0

-- | Ensure that a value has a given kind
--
--   TODO :: change to assertValueKind to make the naming a bit more consistent
assertValKind' :: Int -> Ref Value -> ValKind -> EDGMonad ()
assertValKind' d v k = errContext context $ do
  mvi <- uses @GS valInfo (Map.lookup v)
  vi <- case mvi of
    Nothing -> throw $ "No info found for value at ref `" ++ show v ++ "`"
    Just v -> return v
  let vr = vi ^. valRef
  case (k,vr) of
    -- Each pair:
    --  first :: does nothing if types are matching
    --  second :: if the value has no type and we are asserting a concrete
    --     type, make sure that every value that was previously in our EqClass
    --     now has that concrete ValRef
    (Int    (), Int    _) -> return ()
    (Int    (), KVBot eq) -> generateDatum eq ref Int
    (Bool   (), Bool   _) -> return ()
    (Bool   (), KVBot eq) -> generateDatum eq ref Bool
    (Float  (), Float  _) -> return ()
    (Float  (), KVBot eq) -> generateDatum eq ref Float
    (String (), String _) -> return ()
    (String (), KVBot eq) -> generateDatum eq ref String
    (UID    (), UID    _) -> return ()
    (UID    (), KVBot eq) -> generateDatum eq ref UID
    -- Even if we already have records, we need to make sure they update
    -- properly with any possible new fields.
    (Record rc, Record r) -> assertRecordEqCl' d r rc
    -- If we're creating a new record, we've got to both create new ValRer
    -- and make sure the fields are setup right, we do this by punting to
    -- ourselves.
    (Record rc, KVBot eq) -> do
      generateDatum eq ref Record
      assertValKind' d v k
    -- IF there's no contraint, there's no contraint
    (KVBot (),_) -> return ()
    (KVTop (),_) -> throw $ "Tried to assert that kind of `" ++ show v ++ "` "
      ++ "is KVTop. This is kinda silly."
    (_,KVTop v) -> absurd v
    _ -> throw $ "Tried asserting that `" ++ show v ++ "` has kind `" ++
          show k ++ "` but it has conflicting info `" ++ show vi ++ "`."
  where
    context = "assertValKind `" ++ show v ++ "` `" ++ show k
      ++ "`"
    -- | Take every instance of that EQ class in the valInfo map and generate
    --   a new valInfo with a concrete ValRef there.
    generateDatum :: ValEqClass
                  -> (String -> EDGMonad a)
                  -> (a -> ValRef)
                  -> EDGMonad ()
    generateDatum eq ref cons = errContext context $ do
      vi  <- use @GS valInfo
      vi' <- mapM updateValInfo vi
      valInfo @GS .= vi'
      where
        context = "replacing eq class `" ++ show eq ++ "` with kind `"
          ++ show k ++ "`"
        updateValInfo :: ValInfo -> EDGMonad ValInfo
        updateValInfo i@ValInfo{..}
          | KVBot eqv <- viValRef, eqv == eq = errContext context $ do
            r <- cons <$> (ref . valDataName . unpack $ v)
            return i{viValRef = r}
          | otherwise = return $ i
          where
            context = "updating `" ++ show i ++ "` with new kind `" ++ show k
              ++ "`"

-- | Ensure that two values have the same kind
assertValKindEq :: Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq = assertValKindEq' 0

-- | Ensure that two values have the same kind
assertValKindEq' :: Int ->  Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq' d a b = errContext context $ do
  avi <- getVI a
  bvi <- getVI b
  let avr = avi ^. valRef
      bvr = bvi ^. valRef
  case (avr,bvr) of
    --
    (Int     _,  _       ) -> assertBoth $ Int ()
    (_        , Int    _ ) -> assertBoth $ Int ()
    --
    (Bool     _,  _       ) -> assertBoth $ Bool ()
    (_        , Bool    _ ) -> assertBoth $ Bool ()
    --
    (Float     _,  _       ) -> assertBoth $ Float ()
    (_        , Float    _ ) -> assertBoth $ Float ()
    --
    (String     _,  _       ) -> assertBoth $ String ()
    (_        , String    _ ) -> assertBoth $ String ()
    --
    (UID    _,  _        ) -> assertBoth $ UID ()
    (_        , UID    _ ) -> assertBoth $ UID ()
    --
    (Record ar, Record br) -> do
      -- merge the equalitity classes and create missing fields
      eqa <- getRecEqCl ar
      eqb <- getRecEqCl br
      joinRecordEqCl' d eqa eqb
      -- Get the current fields for the records and force them to share a kind
      rfa <- (^. fields) <$> getRecInfo ar
      rfb <- (^. fields) <$> getRecInfo br
      let rfPairs = Map.intersectionWith (\ (_,a) (_,b) -> (a,b)) rfa rfb
          rfMatch = Map.keysSet rfa == Map.keysSet rfb
      when (not rfMatch) $ throw $ "Records don't have matching sets of fields"
        ++ ".\n  Record `" ++ show ar ++ "` has fields `" ++ show rfa ++ "`"
        ++ "\n  Record `" ++ show br ++ "` has fields `" ++ show rfb ++ "`"
      traverse (uncurry $ assertValKindEq' d) rfPairs
      return ()
    (Record ar, _) -> do
      aeq <- getRecEqCl ar
      assertValKind' d b (Record aeq)
    (_, Record br) -> do
      beq <- getRecEqCl br
      assertValKind' d a (Record beq)
    --
    (KVTop v,_) -> absurd v
    (_,KVTop v) -> absurd v
    --
    (KVBot eqo, KVBot eqn) -> replaceEqClass eqo eqn
  where
    assertBoth k = do
      assertValKind' d a k
      assertValKind' d b k

    context = "assertValKindEq `" ++ show a ++ "` `" ++ show b ++ "`"

    getVI r = do
      mrvi <- uses @GS valInfo (Map.lookup r)
      case mrvi of
        Nothing -> throw $ "No info found for value at ref `" ++ show r ++ "`"
        Just v -> return v

    -- | Take every instance of the first Eq class and replace it with the
    --   second so that we preserve transitive closure.
    replaceEqClass eqo eqn = errContext context $ do
      vi  <- use @GS valInfo
      vi' <- mapM updateEqClass vi
      valInfo @GS .= vi'
      where
        context = "replaceEqClass `" ++ show eqo ++ "` `" ++ show eqn ++ "`"
        updateEqClass :: ValInfo -> EDGMonad ValInfo
        updateEqClass i@ValInfo{..}
          | KVBot eqv <- viValRef, eqv == eqo = return i{viValRef = KVBot eqn}
          | otherwise = return $ i

-- Join two record equivalence classes.
joinRecordEqCl :: RecEqClass -> RecEqClass -> EDGMonad RecEqClass
joinRecordEqCl = joinRecordEqCl' 0

-- Join two record equivalence classes.
joinRecordEqCl' :: Int -> RecEqClass -> RecEqClass -> EDGMonad RecEqClass
joinRecordEqCl' d eqa eqb
  -- Depth check
  | d > maxDepth = errContext context $ throw $ "recursion depth of "
    ++ show d ++ "reached. Aborting due to likely cycle in record field "
    ++ "instantiations."
  -- Combine the two EqClasses, merging the neccesary fields, and
  -- set all records with the original eqClasses to the new one.
  | otherwise = errContext context $ do
    let d' = d + 1
    -- Get the kinds
    ka <- getRecKind eqa
    kb <- getRecKind eqb
    -- Get the new combined eqclass
    eqn <- errContext ("joinRecordEqCl ka: " ++ show ka ++ " kb: " ++
      show kb) $ combineKinds' d' ka kb
    -- Replace the elements with that new eqclass
    replaceKind' d' eqa eqn
    replaceKind' d' eqb eqn
    -- return the new eqclass
    return eqn
  where
    context = "joinRecordEqCl `" ++ show eqa ++ "` `" ++ show eqb ++ "`"
    -- The maximum allowed recursive depth.
    maxDepth = 20

-- | gets the kind of a record given the eq class
getRecKind :: RecEqClass -> EDGMonad RecKind
getRecKind eq = errContext context $ do
  mk <- uses @GS recordKinds (Map.lookup eq)
  case mk of
    Nothing -> throw $ "could not find kind for Equality class `"
      ++ show eq ++ "`"
    Just k -> return k
  where
    context = "getRecKind `" ++ show eq ++ "`"

-- | Get the information for a record.
getRecInfo :: Ref Record -> EDGMonad RecInfo
getRecInfo r = errContext context $ do
  -- Get the record info
  mri <- uses @GS recInfo (Map.lookup r)
  case mri of
    Just ri -> return ri
    Nothing -> throw $ "No recordInfo for record `" ++ show r ++ "`"
  where
    context = "getRecInfo `" ++ show r ++ "`"

-- | get equality class.
getRecEqCl :: Ref Record -> EDGMonad RecEqClass
getRecEqCl r = errContext context $ do
  eqcl <- (^. eqClass) <$> getRecInfo r
  errContext (context ++ " '" ++ show eqcl ++ "`") $ return eqcl
  where
    context = "getRecEqCl `" ++ show r ++ "`"

-- gets the kind for a record from a reference
getRecKindFromRef :: Ref Record -> EDGMonad RecKind
getRecKindFromRef r = errContext context $ do
  RecInfo{..} <- getRecInfo r
  getRecKind riEqClass
  where
    context = "getRecKindFromRef `" ++ show r ++ "`"

-- | given two kinds calculates a new combined kind for them
combineKinds :: RecKind -> RecKind -> EDGMonad RecEqClass
combineKinds = combineKinds' 0

-- | given two kinds calculates a new combined kind for them
combineKinds' :: Int -> RecKind -> RecKind -> EDGMonad RecEqClass
combineKinds' d ka kb = errContext context $ do
      -- Unique fields in a
  let ufa = Map.difference ka kb
      -- Unique fields in b
      ufb = Map.difference kb ka
      -- Fields in only one kind
      uf = Map.union ufa ufb
      -- Pairs of types for keys in both kinds.
      sp = Map.intersectionWith (,) ka kb -- :: Map String (ValKind,ValKind)
  -- Iteratoe over and combine all the shared fields.
  sf <- Map.traverseWithKey (\ k v -> errContext (context ++ "\n  Field `"
      ++ k ++ "`") $ (uncurry $ intersectKinds' d) v) sp
  -- Make sure all the fields are added together and grab the new kind
  let af = Map.union sf uf
  -- add the new kind to the map and return the new eqClass
  neq <- addRecordKind af
  errContext ("combineKinds af: " ++ show af ++ " neq: " ++ show neq) $
    return neq
  where
    context = "combineKinds d:`" ++ show d ++ "` ka:`" ++ show ka ++ "` kb:`"
      ++ show kb ++ "`"

-- | combine two ValKinds to get a third, with all the needed changes to
--   the existing set of types. Should be fine if types are non-recursive.
intersectKinds :: ValKind -> ValKind -> EDGMonad ValKind
intersectKinds = intersectKinds' 0

-- | combine two ValKinds to get a third, with all the needed changes to
--   the existing set of types. Should be fine if types are non-recursive.
intersectKinds' :: Int -> ValKind -> ValKind -> EDGMonad ValKind
intersectKinds' d vka vkb
  -- Error cases for KVTop
  | KVTop () <- vka = errContext context . throw $ "Found an invalid "
    ++ "KVTop () when checking kind for ka `" ++ show vka ++ "`"
  | KVTop () <- vkb = errContext context . throw $ "Found an invalid "
    ++ "KVTop () when checking kind for kb `" ++ show vkb ++ "`"
  -- Simple ones, where we're just checking equality
  | Int    () <- vka, Int    () <- vkb = return $ Int    ()
  | Bool   () <- vka, Bool   () <- vkb = return $ Bool   ()
  | Float  () <- vka, Float  () <- vkb = return $ Float  ()
  | String () <- vka, String () <- vkb = return $ String ()
  | UID    () <- vka, UID    () <- vkb = return $ UID    ()
  -- The record combination forces us to recurse and make sure we're
  -- doing it right.
  | Record reqa <- vka, Record reqb <- vkb = errContext context $
    Record <$> joinRecordEqCl' d reqa reqb
  -- When one of the elements is a bottom use the other kind.
  | KVBot () <- vka = return vkb
  | KVBot () <- vkb = return vka
  -- Remaining is cases where there is a kind mismatch.
  | otherwise = errContext context $ throw $ "kinds `" ++ show vka
    ++ "` and `" ++ show vkb ++ "` don't match."
  where
    context = "intersectKinds `" ++ show d ++ "` `" ++ show vka ++ "` `"
      ++ show vkb ++ "`"

-- | Goes through all records, and if a record has the old kind, replaces
--   it with the new kind, and run checks to make sure all values are
--   initialized correctly.
replaceKind :: RecEqClass -> RecEqClass -> EDGMonad ()
replaceKind = replaceKind' 0

-- | Goes through all records, and if a record has the old kind, replaces
--   it with the new kind, and run checks to make sure all values are
--   initialized correctly.
replaceKind' :: Int -> RecEqClass -> RecEqClass -> EDGMonad ()
replaceKind' d eqo eqn = errContext context $ do
  -- Replace instances of eqo in recInfo
  recInfo @GS %= Map.mapWithKey repSingleRecInfo
  -- Replaces instances of eqo in recordKinds
  recordKinds @GS %= Map.map (Map.map repValKind)
  -- Delete the old kind
  errContext ("Deleting Kind # " ++ show eqo) $
    recordKinds @GS %= Map.delete eqo
  -- Ensure all the fields have the correct values created.
  ris <- use @GS recInfo
  errContext ("replaceKind ris: " ++ show ris) $
    Map.traverseWithKey (condCreateFields d) ris
  return ()
  where
    -- Replace the thing inside a valkind
    repValKind :: ValKind -> ValKind
    repValKind r
      | Record eq <- r, eq == eqo = Record eqn
      | otherwise = r

    context = "replaceKind `" ++ show d ++ "` `" ++ show eqo ++ "` `"
      ++ show eqn ++ "`"

    -- | if the recInfo points to our old element, replace it with the new
    --   one.
    repSingleRecInfo :: Ref Record -> RecInfo -> RecInfo
    repSingleRecInfo r ri@RecInfo{..}
      | riEqClass == eqo = ri{riEqClass=eqn}
      | otherwise = ri

    -- | if the record has the new eqClass then make create the fields
    --   if needed.
    condCreateFields :: Int -> Ref Record -> RecInfo -> EDGMonad ()
    condCreateFields d r ri@RecInfo{..}
      | riEqClass == eqn = errContext context $ createFields' d r
      | otherwise = errContext (context ++ "!!nop") $ return ()
      where
        context = "condCreateFields `" ++ show d ++ "` `" ++ show r ++ "`"

-- | makes sure all the variables are correctly initialise for a given
--   record. Recursively verifies that the kinds of things are correct.
createFields :: Ref Record -> EDGMonad ()
createFields = createFields' 0

-- | makes sure all the variables are correctly initialise for a given
--   record. Recursively verifies that the kinds of things are correct.
createFields' :: Int -> Ref Record -> EDGMonad ()
createFields' d r = errContext context $ do
  -- Get info and kind
  ri <- getRecInfo r
  rk <- getRecKind $ ri ^. eqClass
  -- and the fields that are initialized
  let rf = ri ^. fields
      rd = Map.difference rf rk
  -- Make sure that there are no initialized fields that don't actually
  -- appear in the new kind. We should only ever be gaining fields.
  when (not $ Map.null rd) $ throw $ "Record `" ++ show r ++ "` has fields"
    ++ " " ++ show (Map.keys rd) ++ " that are not in the kind `"
    ++ show rk ++ "`, this should never happen."
  -- Go through fields in the kind, and if the value exists ensure it
  -- has the correct kind, if it doesn't exist initialize it properly.
  mapM_ (makeField d r) $ Map.keysSet rk
  where
    context = "createFields `" ++ show d ++ "` `" ++ show r ++ "`"

    -- Given a particular record, make sure the field exists, if not
    -- initialize it, then ensure that it has the correct type.
    makeField :: Int -> Ref Record -> String -> EDGMonad ()
    makeField d r fn = errContext context $ do
      ri <- getRecInfo r
      -- get a kind for the field
      mrfk <- Map.lookup fn <$> getRecKindFromRef r
      rfk <- case mrfk of
        Nothing -> throw $ "No field `" ++ fn ++ "` found in record `"
          ++ show r ++ "` this should never happen."
        Just fk -> return fk
      -- get the map of fields
      let rf = ri ^. fields
      -- get the references to the used flag and the value
      (ur, vr) <- case Map.lookup fn rf of
        Nothing -> do
          -- gen the refs
          u <- ref $ recUsedName  (unpack r) fn
          v <- ref $ recValueName (unpack r) fn
          -- update the field with the new information
          recInfo @GS %= Map.adjust (fields %~ Map.insert fn (u,v)) r
          return (u,v)
        Just t -> return t
      -- Assert that the values have the correct type.
      assertValKind vr rfk
      where
        context = "makeField `" ++ show d ++ "` `" ++ show r ++ "` `"
          ++ fn ++ "`"


-- | Given two record equality classes get their join, replace both original
--   classes with the new one, and create all the variables as needed.
joinRecord :: Ref Record -> Ref Record -> EDGMonad ()
joinRecord = joinRecord' 0

-- | Given two record equality classes get their join, replace both original
--   classes with the new one, and create all the variables as needed.
joinRecord' ::Int -> Ref Record -> Ref Record -> EDGMonad ()
joinRecord' d ra rb = errContext context $ do
  eqa <- (^. eqClass) <$> getRecInfo ra
  eqb <- (^. eqClass) <$> getRecInfo rb
  joinRecordEqCl' d eqa eqb
  return ()
  where
    context = "joinRecord `" ++ show ra ++ "` `" ++ show rb ++ "`"

-- | Ensure that a particular record has a kind that encapsulates the given
--   one
assertRecordEqCl :: Ref Record -> RecEqClass -> EDGMonad ()
assertRecordEqCl = assertRecordEqCl' 0

-- | Ensure that a particular record has a kind that encapsulates the given
--   one
assertRecordEqCl' :: Int -> Ref Record -> RecEqClass -> EDGMonad ()
assertRecordEqCl' d r eq = errContext context $ do
  ri@RecInfo{..} <- getRecInfo r
  joinRecordEqCl' d eq riEqClass
  return ()
  where
    context = "assertRecordEqCL `" ++ show r ++ "` `" ++ show eq ++ "`"

-- | Ensure that a particular record has a kind that encapsulates the given
--   one
assertRecordKind :: Ref Record -> RecKind -> EDGMonad ()
assertRecordKind = assertRecordKind' 0

-- | Ensure that a particular record has a kind that encapsulates the given
--   one
assertRecordKind' :: Int -> Ref Record -> RecKind -> EDGMonad ()
assertRecordKind' d r k = errContext context $ do
  ec <- addRecordKind k
  assertRecordEqCl' d r ec
  where
    context = "assertRecordKind `" ++ show r ++ "` `" ++ show k ++ "`"

valDataName :: String -> String
valDataName = (++ ".data")

valKindName :: String -> String
valKindName = (++ ".kind")

recUsedName :: String -> String -> String
recUsedName r f = r ++ "." ++ f ++ ".used"

recValueName :: String -> String -> String
recValueName r f = r ++ "." ++ f ++ ".val"

instance SBVAble Value where
  type SBVType Value = ValueSBV
  type RefType Value = Ref Value

  ref :: String -> EDGMonad (Ref Value)
  ref name = errContext context $ do
    eq <- newValEqClass
    kr <- ref . valKindName $ name
    let rf = Ref name
        vr = KVBot eq
    exists <- uses @GS valInfo $ Map.member rf
    case exists of
      True -> return rf
      False -> do
        valInfo @GS %= Map.insert rf ValInfo{viKindRef = kr, viValRef = vr}
        returnAnd rf (errContext context $ sbv rf)
    where
      context = "(ref :: Value) `" ++ name ++ "`"

  lit :: Value -> SBVMonad ValueSBV
  lit (unpack -> v) = errContext context $ do
    let rn = "Literal Value: " ++ show v
    ks <- lit (getKindNum v)
    vs <- case v of
      Int    i -> Int    <$> lit i
      Bool   b -> Bool   <$> lit b
      Float  f -> Float  <$> lit f
      String s -> String <$> lit s
      UID    u -> UID    <$> lit u
      Record r -> Record <$> lit r
      KVTop v -> absurd v
      KVBot v -> absurd v
    return ValueSBV{vsKindSBV = ks, vsValSBV = vs, vsRefName = Just rn}
    where
      context = "creating value literal for `" ++ show v ++ "`"

  sbv :: Ref Value -> SBVMonad ValueSBV
  sbv r = errContext context $ do
    val <- uses @SBVS valueRef (Map.lookup r)
    case val of
      -- If there's a value with this name, just return it
      Just v -> return v
      Nothing -> errContext context $ do
        mvinfo <- uses @SBVS valInfo (Map.lookup r)
        -- Otherwise check if we have information on how to generate the
        -- value.
        vinfo <- case mvinfo of
          Nothing -> error $ "There is no stored information for Value `"
            ++ show r ++ "` cannot create symbolic variable."
          Just v -> return v
        -- If so, get the sbv representations thereof and return them.
        let kr = vinfo ^. kindRef
            vr = vinfo ^. valRef
            rn = unpack r
        ks <- sbv kr
        vs <- valRefSBV vr
        let s = ValueSBV{vsKindSBV = ks, vsValSBV = vs, vsRefName = Just rn}
        add r s
        return s
    where
      context = "(sbv :: Value) `" ++ show r ++ "`"

  add :: Ref Value -> ValueSBV -> SBVMonad ()
  add r s = errContext context $ do
    val <- uses @SBVS valueRef (Map.member r)
    case val of
      True  -> throw $ "Reference to value `" ++ show r ++ "` already exists."
      False -> valueRef @SBVS %= (Map.insert r s)
    where
      context = "(add :: Value) `" ++ show r ++ "` `" ++ show s ++ "` `"

  refConcrete :: String -> Value -> EDGMonad (Ref Value)
  refConcrete n v = do
    r <- defaultRefConcrete n v
    k <- valueKind v
    assertValKind r k
    return r

  isAbstract :: Constrained -> ValueSBV -> SBVMonad (SBV Bool)
  isAbstract (unpack -> c) v@ValueSBV{..}
    | Int    i <- c, Int    s <- vsValSBV = con $ addKind i s
    | Bool   i <- c, Bool   s <- vsValSBV = con $ addKind i s
    | Float  i <- c, Float  s <- vsValSBV = con $ addKind i s
    | String i <- c, String s <- vsValSBV = con $ addKind i s
    | UID    i <- c, UID    s <- vsValSBV = con $ addKind i s
    | Record i <- c, Record s <- vsValSBV = con $ addKind i s
    | KVTop  _ <- c = con . return $ S.literal False
    | KVBot  _ <- c = con . return $ S.literal True
    | KVTop  v <- vsValSBV = absurd v
    | KVBot  _ <- vsValSBV = con . throw $ "Cannot check equality when kind "
        ++ "of value is underspecified."
    | otherwise = con . return $ S.literal False
    where
      addKind :: SBVAble t => Constraints t -> SBVType t -> SBVMonad (SBV Bool)
      addKind i s = do
        ve <- errContext context $ isAbstract i s
        ke <- errContext context $ isConcrete (getKindNum c) vsKindSBV
        errContext ("addKind!! ve: " ++ show ve ++ " ke: " ++ show ke) $
          return $ ve S.&&& ke
        where
          context = "addKind `" ++ show i ++ "` `" ++ show s ++ "`"
      con = errContext ("(isAbstract :: value) `"
        ++ show v ++ "` is equal to `" ++ show c ++ "`.")

  refAbstract :: String -> Constrained -> EDGMonad (Ref Value)
  refAbstract n c = errContext context $ do
    r <- defaultRefAbstract n c
    k <- constrainedKind c
    when (k /= KVBot()) $ assertValKind r k
    return r
    where
      context = "(refAbstract :: Value) `" ++ show n ++ "` `" ++ show c ++ "`"

  fixConcrete :: Value -> EDGMonad Value
  fixConcrete (unpack -> Record r) = pack . Record <$> fixConcrete r
  fixConcrete (unpack -> UID    u) = pack . UID    <$> fixConcrete u
  fixConcrete i = return i

  fixAbstract :: Constrained -> EDGMonad Constrained
  fixAbstract (unpack -> Record r) = pack . Record <$> fixAbstract r
  fixAbstract (unpack -> UID    u) = pack . UID    <$> fixAbstract u
  -- We don't explicity catch instances of (KVTop ()) here since other
  -- locations have more context for us to use when creating an error message.
  fixAbstract i = return i

instance InvertSBV Value where

  extract :: Modelable a => DecodeState -> a -> Ref Value -> Maybe Value
  extract ds model ref = do
    let valInfo = getDSValInfo ds
    vi <- Map.lookup ref valInfo
    kind <- extract ds model (vi ^. kindRef)
    if | kind == getKindNum' (Int ()) -> do
            let Int dref = vi ^. valRef
            d <- extract ds model dref
            return . pack . Int $ d
       | kind == getKindNum' (Bool ()) -> do
            let Bool dref = vi ^. valRef
            d <- extract ds model dref
            return . pack . Bool $ d
       | kind == getKindNum' (Float ()) -> do
            let Float dref = vi ^. valRef
            d <- extract ds model dref
            return . pack . Float $ d
       | kind == getKindNum' (String ()) -> do
            let String dref = vi ^. valRef
            d <- extract ds model dref
            return . pack . String $ d
       | kind == getKindNum' (UID ()) -> do
            let UID dref =  vi ^. valRef
            d <- extract ds model dref
            return . pack . UID $ d
       | kind == getKindNum' (Record undefined) -> do
            let Record dref = vi ^. valRef
            d <- extract ds model dref
            return . pack . Record $ d
       | otherwise -> fail "no valid element"

allEqOp ::  (forall a. S.EqSymbolic a => a -> a -> SBV Bool) -> String
        -> Bool -> Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
allEqOp op opName def a b name = errContext context $ do
  n <- ref name
  assertValKindEq a b
  returnAnd n $ errContext context $ do
    ValueSBV{vsValSBV=as} <- sbv a
    ValueSBV{vsValSBV=bs} <- sbv b
    nv <- sbv n
    let ov = case (as,bs) of
                (Int    aiv, Int    biv) -> op aiv biv
                (Bool   aiv, Bool   biv) -> op aiv biv
                (Float  aiv, Float  biv) -> op aiv biv
                (String aiv, String biv) -> op aiv biv
                (UID    aiv, UID    biv) -> op aiv biv
                (Record aiv, Record biv) -> op aiv biv
                _ -> S.literal def
    constrain $ ov S..== nv
  where
    context = opName ++ " `" ++ show a ++ "` and `"
      ++ show b ++ "`"

instance EDGEquals Value where

  equalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  equalE = allEqOp (S..==) "equalE" False

  unequalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  unequalE = allEqOp (S../=) "unequalE" True

boolUnOp :: (SBV Bool -> SBV Bool) -> String
          -> Ref Value -> String -> EDGMonad (Ref Value)
boolUnOp op opName r name = errContext context $ do
  n <- ref name
  assertValKind r (Bool ())
  assertValKind n (Bool ())
  returnAnd n $ errContext context $ do
    ValueSBV{vsValSBV=rs} <- sbv r
    nv <- sbv n
    ob <- case rs of
      Bool bs -> return $ op bs
      _ -> throw $ "Could not create `" ++ show n ++ "` because `" ++ show r
        ++ "` does not have kind of Bool."
    case nv ^. valSBV of
      Bool nb -> constrain $ nb S..== ob
      _ -> throw $ "Output variable `" ++ show n ++ "` was somehow of "
        ++ "incorrect kind."
  where
    context = opName ++ " `" ++ show r ++ "`"

boolBinOp :: (SBV Bool -> SBV Bool -> SBV Bool) -> String
          -> Ref Value -> Ref Value -> String -> EDGMonad (Ref Value)
boolBinOp op opName a b name = errContext context $ do
  n <- ref name
  assertValKind n (Bool ())
  assertValKind a (Bool ())
  assertValKind b (Bool ())
  returnAnd n $ errContext context $ do
    ValueSBV{vsValSBV=av} <- sbv a
    ValueSBV{vsValSBV=bv} <- sbv b
    nv <- sbv n
    ob <- case (av, bv) of
      (Bool ab,Bool bb) -> return $ op ab bb
      _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
           "`" ++ show a ++ "` and `" ++ show b ++ "` have invalid kinds."
    case nv ^. valSBV of
      Bool nb -> constrain $ nb S..== ob
      _ -> throw $ "Output variable `" ++ show n ++ "` was somehow of "
        ++ "incorrect kind."
  where
    context = opName ++ " `" ++ show a ++ "` `" ++ show b ++ "`"

-- | The EDGLogic instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which is doesn't have an EDGLogic instance
instance EDGLogic Value where

  notE :: Ref Value ->  String -> EDGMonad (RefType Value)
  notE = boolUnOp (S.bnot) "notE"

  andE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  andE = boolBinOp (S.&&&) "andE"

  orE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  orE = boolBinOp (S.|||) "orE"

  impliesE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  impliesE = boolBinOp (S.==>) "impliesE"

  nandE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  nandE = boolBinOp (S.~&) "nandE"

  norE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  norE = boolBinOp (S.~|) "norE"

  xorE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  xorE = boolBinOp (S.<+>) "xorE"

ordBinOp :: (forall a. S.OrdSymbolic a => a -> a -> SBV Bool) -> String
         -> Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
ordBinOp op opName a b name = errContext context $ do
  n <- ref name
  assertValKindEq a b
  returnAnd n $ errContext context $ do
    ValueSBV{vsValSBV=av} <- sbv a
    ValueSBV{vsValSBV=bv} <- sbv b
    nb <- sbv n
    ob <-case (av,bv) of
      (Int ai, Int bi) -> return $ op ai bi
      (Float af, Float bf) -> return $ op af bf
      _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
           "`" ++ show a ++ "` and `" ++ show b ++ "` have invalid"
           ++  " kinds, both kinds must be identical and numeric."
    constrain $ nb S..== ob
  where
    context = opName ++ " `" ++ show a ++ "` `" ++ show b ++ "`"

-- | The EDGOrd instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which doesn't have an EDGLogic instance
instance EDGOrd Value where

  gtE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gtE = ordBinOp (S..>) "gtE"

  gteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gteE = ordBinOp (S..>=) "gteE"

  ltE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  ltE = ordBinOp (S..<) "ltE"

  lteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  lteE = ordBinOp (S..<=) "lteE"

instance S.EqSymbolic RecSBV where

  (.==) a@RecSBV{rsFields=ma} b@RecSBV{rsFields=mb}
    = uaCorrect S.&&& ubCorrect S.&&& intersectionCorrect
    where
      -- Are all the unique fields in A unused?
      uaCorrect = let mua = Map.difference ma mb in
        S.bAll unusedField (Map.elems mua)
      -- Are all the unique fields in B unused?
      ubCorrect = let mub = Map.difference mb ma in
        S.bAll unusedField (Map.elems mub)
      -- Is the field unused?
      unusedField (br,_) = br S..== (S.literal False)
      -- Are all of the shared fields correct
      intersectionCorrect = S.bAnd . Map.elems $
          Map.intersectionWith combineFields ma mb
      -- Make sure both fields are in use *and* the values are identical.
      combineFields (abr,avr) (bbr,bvr) = (abr S..== bbr) S.&&& (avr S..== bvr)

  (./=) a@RecSBV{rsFields=ma} b@RecSBV{rsFields=mb}
    = uaCorrect S.&&& ubCorrect S.&&& intersectionCorrect
    where
      -- Are any of the unique fields in A used?
      uaCorrect = let mua = Map.difference ma mb in
        S.bAny usedField (Map.elems mua)
      -- Are any of the unique fields in B unused?
      ubCorrect = let mub = Map.difference mb ma in
        S.bAny usedField (Map.elems mub)
      -- Is the field used?
      usedField (br,_) = br S..== (S.literal True)
      -- Are all of the shared fields correct
      intersectionCorrect = S.bOr . Map.elems $
          Map.intersectionWith combineFields ma mb
      -- Make sure fields don't have the same usage state *or* the values are
      -- different.
      combineFields (abr,avr) (bbr,bvr) = (abr S../= bbr) S.||| (avr S../= bvr)

instance SBVAble Record where
  type SBVType Record = RecSBV
  type RefType Record = Ref Record

  ref :: String -> EDGMonad (Ref Record)
  ref name = errContext context $ do
    -- get the new ref, make sure it exists.
    let rf  = Ref name
    mr <- uses @GS recInfo $ Map.lookup rf
    case mr of
      Just _ -> return rf
      Nothing -> errContext context $ do
        let fs = Map.empty
            kn = Map.empty
        -- get the eqclass for the new kinds
        eq <- addRecordKind kn
        -- insert it into the usual map
        recInfo @GS %= Map.insert rf RecInfo{riFields=fs, riEqClass=eq}
        returnAnd rf $ errContext context $ sbv rf
    where
      context = "(ref :: Record) `" ++ name ++ "`"

  lit :: Record -> SBVMonad RecSBV
  lit (unpack -> r) = errContext context $ do
    fls <- traverse buildField r
    let name = "Literal Record: " ++ show r
    return $ RecSBV{rsFields = fls, rsRefName = Just name}
    where
      buildField :: Value -> SBVMonad (SBV Bool, ValueSBV)
      buildField v = do
        vs <- lit v
        return (S.literal True,vs)
      context = "(lit :: Record) `" ++ show r ++ "`"

  sbv :: Ref Record -> SBVMonad RecSBV
  sbv r = errContext context $ do
    val <- uses @SBVS recordRef (Map.lookup r)
    case val of
      Just v -> return v
      Nothing -> do
        ri <- getRecInfoSBV r
        rk <- getRecKindSBV (ri ^. eqClass)
        let name = unpack r
            rf = ri ^. fields
            fmatch = Map.keysSet rk == Map.keysSet rf
        when (not fmatch) $ throw $ "In record `" ++ show r ++ "` the set of "
          ++ "fields in the valInfo `" ++ show rf ++ "` and the fields in the "
          ++ "kind table `" ++ show rk ++ "` do not match."
        fls <- traverse genBoolAndValSBV rf
        let rs = RecSBV{rsFields=fls,rsRefName=Just name}
        errContext ("fmatch : " ++ show fmatch) $ add r rs
        return rs
    where
      context = "(sbv :: Record) `" ++ show r ++ "`"

      genBoolAndValSBV :: (Ref Bool,Ref Value)
                       -> SBVMonad (SBV Bool, ValueSBV)
      genBoolAndValSBV (rb,rv) = do
        sb <- sbv rb
        rv <- sbv rv
        return (sb,rv)

      getRecKindSBV :: RecEqClass -> SBVMonad RecKind
      getRecKindSBV eq = errContext context $ do
        mk <- uses @SBVS recordKinds (Map.lookup eq)
        case mk of
          Nothing -> throw $ "No record kind found for equality class `"
            ++ show eq ++ "`"
          Just k -> return k
        where
          context = "getRecKindSBV `" ++ show eq ++ "`"

  add :: Ref Record -> RecSBV -> SBVMonad ()
  add r s = errContext context $ do
    val <- uses @SBVS recordRef (Map.member r)
    case val of
      True  -> throw $ "Reference to record `" ++ show r ++ "` already exists."
      False -> recordRef @SBVS %= (Map.insert r s)
    where
      context = "(add :: Record) `" ++ show r ++ "` `" ++ show s ++ "`"

  isAbstract :: RecCons -> RecSBV -> SBVMonad (SBV Bool)
  isAbstract rc s@RecSBV{..}
    | RCTop <- rc = errContext context $ return $ S.literal False
    | RCBottom <- rc = errContext context $ return $ S.literal True
    | RCAmbig{..} <- rc , Map.null rcMap = errContext context $
        return $ S.literal True
    | RCAmbig{..} <- rc = errContext context $ do
      let unusedFs = Map.elems $ Map.difference rsFields rcMap -- unused fields
          -- No fields in the spec which aren't in the RecSBV
          md = Map.difference rcMap rsFields
          noUnavail = Map.null $ md
          -- get all the fields we use along with corresponding constraints
          usedFs = Map.elems $ Map.intersectionWith (,) rsFields rcMap
          -- Is ever field that's unique to the recSBV unused?
          allUnused = S.bAll (\ (rb,_) -> rb S..== S.literal False) unusedFs
      -- Is every field that's shared both used and satifying the constraint?
      allEq <- S.bAnd <$> (flip traverse usedFs $ \ ((rb,rv),av) -> do
        -- For each field, check if we're actually using it
        let isUsed = rb S..== S.literal True
        -- and if we satisfy the constraint the record places on it.
        isMatch <- isAmbiguous av rv
        return $ isUsed S.&&& isMatch)
      when (not noUnavail) (throw $ "There exists Fields in the recordCons `"
        ++ show rcMap ++ "` which are not in the pregenerated fields `"
        ++ show rsFields ++ "` for this record")
      -- Are all 3 of our major constraints true?
      errContext ("isAbsRec!! au:" ++ show allUnused ++ " ae:" ++ show allEq
        ++ " nou:" ++ show noUnavail ++ show md ) $ return $ allUnused S.&&& allEq
    where
      context = "(isAbstract :: Record) '" ++ show rc ++ "` `" ++ show s ++ "`"

  refAbstract :: String -> RecCons -> EDGMonad (Ref Record)
  refAbstract name c
    | RCTop <- c = errContext context $ throw $ "Cannot create a reference `"
      ++ name ++ "` for unsatsfiable contraint `" ++ show c ++ "`"
    | RCBottom <- c = errContext context $ ref name
    | RCAmbig{..} <- c = errContext context $ do
      -- Get the base record w/ the extra contrainsts applied
      r <- defaultRefAbstract name c
      -- Get the kind of the cosntraints
      k <- recConsKind c
      -- Assert the new record has that kind
      assertRecordKind r k
      -- return the original ref.
      return r
    where
      context = "(refAbstract :: Record) `" ++ name ++ "` `" ++ show c ++ "`"

  fixAbstract :: RecCons -> EDGMonad RecCons
  -- fix all the elements of the constraint.
  fixAbstract RCAmbig{..} = RCAmbig <$> traverse fixAmbiguous rcMap
  fixAbstract i = return i

instance InvertSBV Record where

  extract :: Modelable a => DecodeState -> a -> Ref Record -> Maybe Record
  extract ds model ref = do
    let recInfo = getDSRecInfo ds
    ri <- Map.lookup ref recInfo
    let rm = Map.mapMaybe getRF (ri ^. fields)
    return . pack $ rm

    where
      getRF :: (Ref Bool,Ref Value) -> Maybe Value
      getRF (rb,rv) = do
        b <- extract ds model rb
        when (not b) $ fail "Not in record"
        v <- extract ds model rv
        return v

recEqOp :: (RecSBV -> RecSBV -> SBV Bool)
        -> String
        -> Ref Record
        -> Ref Record
        -> String
        -> EDGMonad (Ref Bool)
recEqOp op opName a b name = errContext context $ do
  r <- mkBinOp op opName a b name
  joinRecord a b
  return r
  where
    context = opName ++ " `" ++ show a ++ "` `" ++ show b ++ "` `" ++ name
      ++ "`"

instance EDGEquals Record where

  equalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  equalE = recEqOp (S..==) "equalE"

  unequalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  unequalE = recEqOp (S../=) "unequalE"

getVal :: String -> EDGMonad (Ref Value)
getVal s = ec $ do
   -- Just get a value with that name if possible ...
   --
   -- NOTE :: This is a bit weird, since it'll just create that value if
   --         needed. Whatever, it's kinda neccesary, esp given how the
   --         first pass description of the system is trying to declarative
   --         in instances where there are no errors.
   --
   --         (i.e. the order of top level declarations should not matter
   --         unless there are multiple possible errors in the system in
   --         which case the error returned is unspecified)
  rv <- ref n
  getValL rv l
  where
    ec = errContext context
    context = "(getVal :: String -> EDGMonad (Ref Value)) `" ++ s ++ "`"
    n:l = split '.' s

-- | getValS lets you use a string specifier to get a value reference
getValS :: Ref Value -> String -> EDGMonad (Ref Value)
getValS r ('.':ls) = getValL r (split '.' ls)
getValS r ls = getValL r (split '.' ls)

-- | gets a value from a start value, and a nested list of other values.
getValL :: Ref Value -> [String] -> EDGMonad (Ref Value)
getValL r l
  | [] <- l = ec $ return r
  | f:ls <- l = ec $ do
    r' <- getField r f
    getValL r' ls
  where
    ec = errContext context
    context = "getValL `" ++ show r ++ "` `" ++ show l ++ "`"

-- | Given a value, and a field name, get the value for the field that's being
--   used.
getField :: Ref Value -> String -> EDGMonad (Ref Value)
getField rv f = ec $ do
  eq <- addRecordKind (Map.fromList [(f,KVBot ())])
  assertValKind rv (Record eq)
  mvi <- uses @GS valInfo (Map.lookup rv)
  vi <- case mvi of
    Nothing -> throw $ "No valInfo for `" ++ show rv ++ "`"
    Just v -> return v
  let vr = vi ^. valRef
  rr <- case vr of
    Record r -> return r
    _ -> throw $ "Value `" ++ show rv ++ "` is not a record and cannot have a"
     ++ " field `" ++ show f ++ "`"
  ri <- getRecInfo rr
  let fs = ri ^. fields
  case Map.lookup f fs of
    Nothing -> throw $ "No field `" ++ f ++ "` exists in record `" ++ show rr
      ++ "`"
    Just (_,v) -> errContext (context ++ " ri:" ++ show ri) $ return v
  where
    ec = errContext context
    context = "getField `" ++ show rv ++ "` `" ++ f ++ "`"
