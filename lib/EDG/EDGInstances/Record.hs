
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

import Text.Show.Pretty hiding (Value,Float,String)
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
  constrain s = returnAnd () $ sbv s >>= constrain

-- TODO :: Replace this with more generic versions that use the constraint
--         on the existence of a field in the state. So that we don't make a
--         separate function for each monad.

-- | Get the equality class from the ref to a value
getValEqClassEDG :: Ref Value -> EDGMonad EqClassID
getValEqClassEDG a = do
 meca <- uses @GS valInfo (Map.lookup a)
 case meca of
   Nothing -> throw $ "No Equality class found for Value `" ++ show a ++ "`."
   Just (ValInfo{..}) -> return viEqClass

-- | Get the equality class from the ref to a value
getValEqClassSBV :: Ref Value -> SBVMonad EqClassID
getValEqClassSBV a = do
  meca <- uses @SBVS valInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No Equality class found for Value `" ++ show a ++ "`."
    Just (ValInfo{..}) -> return viEqClass

-- -- | Get the equality class from the ref to a record
getRecEqClassEDG :: Ref Record -> EDGMonad EqClassID
getRecEqClassEDG a = do
  meca <- uses @GS recInfo (Map.lookup a)
  case meca of
    Nothing -> do
      state <- get @GS
      trace (ppShow state)
        (error $ "No Equality class found for Record `" ++ show a ++ "`.")
    Just (RecInfo{..}) -> return riEqClass

-- | Get the equality class from the ref to a record
getRecEqClassSBV :: Ref Record -> SBVMonad EqClassID
getRecEqClassSBV a = do
  meca <- uses @SBVS recInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No Equality class found for Record `" ++ show a ++ "`."
    Just (RecInfo{..}) -> return riEqClass

-- | Get the equality class from the ref to a record
getRecInfoEDG :: Ref Record -> EDGMonad RecInfo
getRecInfoEDG a = do
  meca <- uses @GS recInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No RecInfo found for Record `" ++ show a ++ "`."
    Just v -> return v

-- | Get the equality class from the ref to a record
getRecInfoSBV :: Ref Record -> SBVMonad RecInfo
getRecInfoSBV a = do
  meca <- uses @SBVS recInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No RecInfo found for Record `" ++ show a ++ "`."
    Just v -> return v

-- | Get kind from the equality class
getEqClKindEDG :: String -> EqClassID -> EDGMonad (Kind' EqClassID)
getEqClKindEDG context ec = do
  mk <- uses @GS classKind (Map.lookup ec)
  case mk of
    Just v -> return v
    Nothing -> throw $ "Could not find kind for eqclass `" ++ show ec
      ++ "` within context: " ++ context ++ "."

-- | Get kind from the equality class
getEqClKindSBV :: String -> EqClassID -> SBVMonad (Kind' EqClassID)
getEqClKindSBV context ec = do
  mk <- uses @SBVS classKind (Map.lookup ec)
  case mk of
    Just v -> return v
    Nothing -> throw $ "Could not find kind for eqclass `" ++ show ec
      ++ "` within context: " ++ context ++ "."

-- | Update the various relevant records with a change of eqclass
replaceEqClass :: EqClassID -> EqClassID -> EDGMonad (EqClassID)
replaceEqClass old new = do
  -- TODO :: add filters that remove unused elements.
  valInfo   @GS %= (Map.map updateVIMap)
  recInfo   @GS %= (Map.map updateRIMap)
  classKind @GS %= (Map.map updateClassKind)
  return new
  where
    -- | Function used to update the ValInfo map with new eqclasses
    updateVIMap e@ValInfo{..}
      = if old == viEqClass then e{viEqClass = new} else e

    -- | Function used to update the RecInfo map with new eqclasses
    updateRIMap e@RecInfo{..}
      = if old == riEqClass then e{riEqClass = new} else e

    -- | Function used to update the kind map when needed.
    updateClassKind (Record m)
      = Record $ Map.map (\ e -> if old == e then new else e) m
    updateClassKind a = a

-- Mark that the kinds of these two values are equal, unifiying them as
-- neccesary.
assertValKindEq :: String -> Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq context a b = do
  -- | Get the equality classes of both refs
  eca <- trace ("vke-a:" ++ show a) $ getValEqClassEDG a
  ecb <- trace ("vke-b:" ++ show b) $ getValEqClassEDG b
  if | eca == ecb -> return ()
     | otherwise  -> do
          assertEQCKindEq context ("Value `" ++ show a ++ "`")
            ("Value `" ++ show b ++ "`") eca ecb
          return ()
-- Mark that the kinds of these two records are equal, unifiying them as
-- neccesary.
assertRecKindEq :: String -> Ref Record -> Ref Record -> EDGMonad ()
assertRecKindEq context a b = do
  -- | Get the equality classes of both refs
  eca <- trace ("rke-a:" ++ show a ++ show b ++ " in " ++ context) $ getRecEqClassEDG a
  ecb <- trace ("rke-b:" ++ show b ++ " in " ++ context) $ getRecEqClassEDG b
  if | eca == ecb -> return ()
     | otherwise  -> do
          assertEQCKindEq context ("Record `" ++ show a ++ "`")
            ("Record `" ++ show b ++ "`") eca ecb
          return ()

-- | Given two equality classes, assert that they are equal.
assertEQCKindEq :: String -- Context for this op (error messages)
                -> String -- Name of thing 1 that has kind EqClass 1
                -> String -- Name of thing 2 that has kind EqClass 2
                -> EqClassID -- EqClass 1
                -> EqClassID -- EqClass 2
                -> EDGMonad EqClassID -- The new EqClass for both of them
assertEQCKindEq context as bs eca ecb = do
  ka <- getEqClKindEDG (as ++ " in " ++ context) eca
  kb <- getEqClKindEDG (bs ++ " in " ++ context) ecb
  case (ka,kb) of
    (KVTop (),_) -> throw $ as ++ " has kind `" ++ show ka ++
      "` when trying to unify kinds in context \"" ++ context ++ "\"."
    (_,KVTop ()) -> throw $ bs ++ " has kind `" ++ show kb ++
      "` when trying to unify kinds in context \"" ++ context ++ "\"."
    (KVBot  (), KVBot  ()) -> replaceEqClass eca ecb
    (KVBot  (), kb       ) -> do
      vi <- use @GS valInfo
      nvi <- Map.traverseWithKey (updateBotValInfo eca ecb) vi
      valInfo @GS .= nvi
      replaceEqClass eca ecb
    (ka       , KVBot  ()) -> replaceEqClass ecb eca -- flipped
    (Int    (), Int    ()) -> replaceEqClass eca ecb
    (Bool   (), Bool   ()) -> replaceEqClass eca ecb
    (Float  (), Float  ()) -> replaceEqClass eca ecb
    (String (), String ()) -> replaceEqClass eca ecb
    (UID    (), UID    ()) -> replaceEqClass eca ecb
    (Record ma, Record mb) -> do
      let mua = Map.difference ma mb
          mub = Map.difference mb ma
          ms = Map.intersection ma mb -- Map of shared keys
          mu = Map.union mua mub -- map of unshared keys
      ecn <- newEqClass
      -- get the new eqclasses for all the shared keys
      ms' <- Map.traverseWithKey (assertSharedKey ma mb) ms
      -- combine with with the unshared keys to get the new kind
      let nkind = Record $ Map.union mu ms'
      -- Insert the new EqClass and update each record to use that instead.
      classKind @GS %= Map.insert ecn nkind
      replaceEqClass eca ecn
      replaceEqClass ecb ecn
    _ -> throwKindErr context ka kb
  where
    -- | Given a valinfo and an EqCLass, if the eqclass of the vi matches, then
    --   create the appropriate new element to replace the bottom.
    updateBotValInfo :: EqClassID -> EqClassID
                     -> Ref Value -> ValInfo
                     -> EDGMonad ValInfo
    updateBotValInfo eca ecb r v@ValInfo{..}
      -- Do nothing if no match
      | eca /= viEqClass = return v
      | eca == viEqClass = do
        -- Get the kind of ecb
        kb <- getEqClKindEDG (context ++ show v) ecb
        case kb of
          KVBot () -> error $ "Should never be called when ecb is also KVBot ()"
          Record _ -> do
            let vname = vname' . unpack $ r
            rr <- ref vname
            ri <- getRecInfoEDG rr
            recInfo @GS %= Map.insert rr ri{riEqClass = eca}
            return v{viEqClass=eca,viValRef=Record rr}
          _ -> return v

    -- | Get the shared eq class that is created by combining the keys of two
    --   maps.
    assertSharedKey ma mb k _ = do
      let meca = Map.lookup k ma
          mecb = Map.lookup k mb
      (eca,ecb) <- case (meca,mecb) of
        (Just a,Just b) -> return (a,b)
        (Nothing, _) -> throw $ "No field \"" ++ k ++ "\" in `" ++ as ++ "`."
        (_, Nothing) -> throw $ "No field \"" ++ k ++ "\" in `" ++ bs ++ "`."
      assertEQCKindEq (context ++ " field \"" ++ k ++ "\"")
        (as ++ " field \"" ++ k ++ "\"")
        (bs ++ " field \"" ++ k ++ "\"")
        eca ecb
    -- | The long error declaration we use when there's a kind mismatch.
    throwKindErr context ka kb = throw $ "Attempting to look up current "
      ++ "kind of " ++
      "\n" ++ as ++ " returned kind `" ++ show ka ++ "` and " ++
      "\n" ++ bs ++ " returned kind `" ++ show kb ++ "`" ++
      "\nin context \"" ++ context ++ "\" while trying to assert kind equality."

-- | Set the kind of the value to the given one, as needed
assertValKind :: String -> Ref Value -> Kind' EqClassID -> EDGMonad EqClassID
assertValKind context ref kind = do
  eca <- getValEqClassEDG ref
  -- Just add the new kind to the map and assert equality
  ecb <- newEqClass
  classKind @GS %= Map.insert ecb kind
  assertEQCKindEq context ("Value `" ++ show ref ++ "`")
      ("Arbitrary kind `" ++ show kind ++ "`") eca ecb

-- | Creates a value from a kind, adds it to the list of things.
valFromKind :: Ref Value -> Kind' EqClassID -> SBVMonad ValueSBV
valFromKind r k = do
  ms <- uses @SBVS valueRef (Map.lookup r)
  case ms of
    Just v -> return v
    Nothing -> case k of
      KVTop () -> throw $ "Value `" ++ show r ++ "` has "
        ++ "an unrealizable kind of `KVTop ()`."
      KVBot () -> throw $ "Value `" ++ show r ++ "` has "
        ++ "an ambiguous kind and we cannnot choose a specific realization"
        ++ " for it. Ensure that it is related to something with a fixed"
        ++ " kind at some point in the build process."
      Int    () -> sbvSingleton r k Int
      Bool   () -> sbvSingleton r k Bool
      Float  () -> sbvSingleton r k Float
      String () -> sbvSingleton r k String
      UID    () -> sbvSingleton r k UID
      Record rm -> sbvSingleton r k Record
  where
    -- | Create the relevant variable with the correct constructor
    sbvSingleton r k c = do
      let kref = Ref . kname' . unpack $ r
          vref = Ref . vname' . unpack $ r
          knum = getKindNum k
      -- | Create the kind variable and ensure it's set correctly
      kv <- sbv kref
      kl <- lit knum
      constrain $ kv S..== kl
      -- | Create the value variable
      vv <- sbv vref
      let vs = ValueSBV{vsKindSBV = kv, vsValSBV = c vv}
      -- | Add the new combined thing to the map and return the newly
      --   created variable.
      trace ("vfk: " ++ show r ++ show k) $ add r vs
      return vs

-- | Given a name for a Value Reference gives the name for the data reference
vname' :: String -> String
vname' = (++ ".data")

-- | Given a name for a Value Reference gives the name for the kind reference
kname' :: String -> String
kname' = (++ ".kind")

-- | given a record name and a field name gives you the record value name
rvname' :: String -> String -> String
rvname' rn fn = rn ++ "." ++ fn

instance SBVAble Value where

  type SBVType Value = ValueSBV

  type RefType Value = Ref Value

  -- | Creates a ref with a kind of bottom.
  ref :: String -> EDGMonad (Ref Value)
  ref n = do
    let oref = Ref n
        kind = KVBot ()
        vref = KVBot ()
    kref <- ref $ kname' n
    eqcl <- newEqClass
    let vinfo = ValInfo{
        viEqClass = eqcl
      , viValRef  = vref
      , viKindRef = kref
      }
    exists <- uses @GS valInfo $ Map.member oref
    when exists (throw $ "Value `" ++ show oref ++ "` already exists.")
    valInfo   @GS %= Map.insert oref vinfo
    classKind @GS %= Map.insert eqcl kind
    returnAnd oref (sbvNoDup "Value" valueRef oref)

  sbv :: Ref Value -> SBVMonad ValueSBV
  sbv r = do
    val <- uses @SBVS valueRef (Map.lookup r)
    case val of
      Just v  -> return v
      Nothing -> do
        -- | See whether there's valinfo for this element.
        mvinfo <- uses @SBVS valInfo (Map.lookup r)
        vinfo <- case mvinfo of
          Nothing -> throw $ "No information found for value `" ++ show r
            ++ "`, cannot create the variables needed."
          Just v -> return v
        let kref = viKindRef vinfo
            vref = viValRef vinfo
            eqcl = viEqClass vinfo
        -- | ensure there's a kind
        mkind <- uses @SBVS classKind (Map.lookup eqcl)
        case mkind of
          Just k -> valFromKind r k
          Nothing -> throw $ "No kind found for value `" ++ show r ++ "`, "
            ++ "cannot create the variables needed."

  lit :: Value     -> SBVMonad ValueSBV
  lit v = throw $ "There is no notion of a literal for Values. " ++ show v

  refConcrete :: String -> Value -> EDGMonad (Ref Value)
  refConcrete n (unpack -> v)
    | Int    i <- v = refConcreteSingleton (Int    ()) i Ref
    | Bool   b <- v = refConcreteSingleton (Bool   ()) b Ref
    | Float  f <- v = refConcreteSingleton (Float  ()) f Ref
    | String s <- v = refConcreteSingleton (String ()) s Ref
    | UID    u <- v = refConcreteSingleton (UID    ()) u Ref
    | Record r <- v = refConcreteSingleton (Record Map.empty) r Ref
    -- I love Void. Because it's an uninhabited type, using that value in your
    | KVTop a <- v = absurd a
    | KVBot a <- v = absurd a
      where
        refConcreteSingleton :: (SBVAble f, EDGEquals f)
                             => Kind' EqClassID
                             -> f
                             -> (String -> RefType f)
                             -> EDGMonad (Ref Value)
        refConcreteSingleton kind lit mkRef = do
          -- Create a ref for our element
          r <- ref n
          -- Ensure that its kind is correct
          assertValKind n r kind
          -- Generate the reference we'll use to hold the value leter
          let vname = vname' n
              vref = mkRef $ vname
          -- Generate the reference we'll use to store the concrete value
          u <- newUID
          v <- refConcrete (vname ++ ".temp[" ++ show u ++ "]") lit
          -- Constrain their equality
          isEq <- vref .== v
          constrain isEq
          return r

  refAbstract :: String -> Constrained -> EDGMonad (Ref Value)
  refAbstract n (unpack -> v)
    | Int    i <- v = refAbstractSingleton (Int    ()) i Ref
    | Bool   b <- v = refAbstractSingleton (Bool   ()) b Ref
    | Float  f <- v = refAbstractSingleton (Float  ()) f Ref
    | String s <- v = refAbstractSingleton (String ()) s Ref
    | UID    u <- v = refAbstractSingleton (UID    ()) u Ref
    | Record r <- v = refAbstractSingleton (Record Map.empty) r Ref
    | KVTop  a <- v = throw $ "Cannot create Value `" ++ show n ++ "` as it"
        ++ " is unrealizable."
    | KVBot  a <- v = ref n
      where
        refAbstractSingleton :: (SBVAble f, EDGEquals f)
                             => Kind' EqClassID
                             -> Constraints f
                             -> (String -> RefType f)
                             -> EDGMonad (Ref Value)
        refAbstractSingleton kind lit mkRef = do
          -- Create a ref for our element
          r <- ref n
          -- Ensure that its kind is correct
          assertValKind n r kind
          -- Generate the reference we'll use to hold the value leter
          let vname = vname' n
              vref = mkRef vname
          -- Generate the reference we'll use to store the concrete value
          u <- newUID
          v <- refAbstract (vname ++ ".temp[" ++ show u ++ "]") lit
          -- Constrain their equality
          isEq <- vref .== v
          constrain isEq
          return r

  add :: Ref Value -> ValueSBV -> SBVMonad ()
  add r s = trace ("adding: " ++ show r ++ show s) $ do
    val <- uses @SBVS valueRef (Map.member r)
    case val of
      True  -> error $ "Reference to value `" ++ show r ++ "` already exists."
      False -> valueRef @SBVS %= (Map.insert r s)

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
    let name = unpack ref
        kname = kname' name
        dname = vname' name
        kref = Ref kname
    kind <- extract ds model kref
    if | kind == getKindNum' (Int ()) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . Int $ d
       | kind == getKindNum' (Bool ()) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . Bool $ d
       | kind == getKindNum' (Float ()) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . Float $ d
       | kind == getKindNum' (String ()) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . String $ d
       | kind == getKindNum' (UID ()) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . UID $ d
       | kind == getKindNum' (Record undefined) -> do
            let dref = Ref dname
            d <- extract ds model dref
            return . pack . Record $ d
       | otherwise -> fail "no valid element"


allEqOp ::  (forall a. S.EqSymbolic a => a -> a -> SBV Bool)
        -> (RecSBV -> RecSBV -> SBVMonad (SBV Bool))
        -> Bool -> Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
allEqOp op rop def a b name = do
  n <- ref name
  trace ("aeo: " ++ show a ++ show b ++ name) $ assertValKindEq name a b
  returnAnd n $ do
    akind <- getValueKind a
    bkind <- getValueKind b
    case (akind,bkind) of
      (Int     (), Int     ()) -> do
        (ValueSBV _ (Int aiv)) <- sbv a
        (ValueSBV _ (Int biv)) <- sbv b
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (Bool    (), Bool    ()) -> do
        (ValueSBV _ (Bool aiv)) <- sbv a
        (ValueSBV _ (Bool biv)) <- sbv b
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (Float   (), Float   ()) -> do
        (ValueSBV _ (Float aiv)) <- sbv a
        (ValueSBV _ (Float biv)) <- sbv b
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (String  (), String  ()) -> do
        (ValueSBV _ (String aiv)) <- sbv a
        (ValueSBV _ (String biv)) <- sbv b
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (UID     (), UID     ()) -> do
        (ValueSBV _ (UID aiv)) <- sbv a
        (ValueSBV _ (UID biv)) <- sbv b
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (Record aeq, Record beq) -> do
        (ValueSBV _ (Record aiv)) <- sbv a
        (ValueSBV _ (Record biv)) <- sbv b
        cv <- rop aiv biv
        ov <- sbv n
        constrain $ ov S..== cv
      -- TODO :: Add error catching for KVTop and KVBot. Those should never
      --         ever get this far, but you know how it goes.
      _ -> do nv <- sbv n
              lv <- lit def
              constrain $ nv S..== lv

instance EDGEquals Value where

  equalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  equalE = allEqOp (S..==) (recEq True) False

  unequalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  unequalE = allEqOp (S../=) (undefined) True

boolUnOp :: (SBV Bool -> SBV Bool)
          -> Ref Value -> String -> EDGMonad (Ref Value)
boolUnOp op r name = do
  n <- refAbstract @Value name (pack $ Bool bottom)
  returnAnd n $ do
    rkind <- getValueKind r
    case rkind of
      Bool{} -> do
        (ValueSBV _ (Bool riv)) <- sbv r
        (ValueSBV _ (Bool niv)) <- sbv n
        constrain $ riv S..== (op niv)
      _ -> throw $ "Could not create `" ++ show n ++ "` because `" ++ show r
        ++ "` does not have kind of Bool."

boolBinOp :: (SBV Bool -> SBV Bool -> SBV Bool)
          -> Ref Value -> Ref Value -> String -> EDGMonad (Ref Value)
boolBinOp op a b name = do
  n <- refAbstract @Value name (pack $ Bool bottom)
  returnAnd n $ do
    akind <- getValueKind a
    bkind <- getValueKind b
    case (akind, bkind) of
      (Bool (),Bool ()) -> do
        (ValueSBV _ (Bool aiv)) <- sbv a
        (ValueSBV _ (Bool biv)) <- sbv b
        (ValueSBV _ (Bool niv)) <- sbv n
        constrain $ niv S..== (op aiv biv)
      _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
           "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
           "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
           "at least one of which is not Bool."

-- | The EDGLogic instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which is doesn't have an EDGLogic instance
instance EDGLogic Value where

  notE :: Ref Value ->  String -> EDGMonad (RefType Value)
  notE = boolUnOp (S.bnot)

  andE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  andE = boolBinOp (S.&&&)

  orE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  orE = boolBinOp (S.|||)

  impliesE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  impliesE = boolBinOp (S.==>)

  nandE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  nandE = boolBinOp (S.~&)

  norE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  norE = boolBinOp (S.~|)

  xorE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  xorE = boolBinOp (S.<+>)

ordBinOp :: (forall a. S.OrdSymbolic a => a -> a -> SBV Bool)
         -> Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
ordBinOp op a b name = do
  n <- ref name
  assertValKindEq name a b
  returnAnd n $ do
    akind <- getValueKind a
    bkind <- getValueKind b
    case (akind,bkind) of
      (Int     (), Int     ()) -> do
        (ValueSBV _ (Int aiv)) <- sbv a
        (ValueSBV _ (Int biv)) <- sbv a
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      (Float   (), Float   ()) -> do
        (ValueSBV _ (Float aiv)) <- sbv a
        (ValueSBV _ (Float biv)) <- sbv a
        ov <- sbv n
        constrain $ ov S..== (op aiv biv)
      _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
           "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
           "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
           ", both kinds must be identical and numeric."

-- | The EDGOrd instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which doesn't have an EDGLogic instance
instance EDGOrd Value where

  gtE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gtE = ordBinOp (S..>)

  gteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gteE = ordBinOp (S..>=)

  ltE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  ltE = ordBinOp (S..<)

  lteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  lteE = ordBinOp (S..<=)

addField :: Ref Record      -- Record we're adding the field to
         -> String          -- Field Name
         -> Ambiguous Value -- The value we're inserting.
         -> EDGMonad ()
addField = undefined

instance SBVAble Record where

  type SBVType Record = RecSBV

  type RefType Record = (Ref Record)

  ref :: String -> EDGMonad (Ref Record)
  ref name = do
    let oref = Ref name
        kind = Record Map.empty
    eqcl <- newEqClass
    exists <- uses @GS recInfo $ Map.member oref
    when exists (throw $ "Record '" ++ show oref ++ "` already exists.")
    recInfo   @GS %= Map.insert oref RecInfo{riFields=Map.empty,riEqClass=eqcl}
    classKind @GS %= Map.insert eqcl kind
    returnAnd oref (sbv oref)

  sbv :: Ref Record -> SBVMonad RecSBV
  sbv r = do
    val <- uses @SBVS recordRef (Map.lookup r)
    case val of
      Just v -> return v
      Nothing -> do
        RecInfo{riFields=fm,riEqClass=eqcl} <- getRecInfoSBV r
        kind <- getEqClKindSBV ("Looking up kind to create Record `" ++ show r
          ++ "`.") eqcl
        case kind of
          KVTop () -> throw $ "Record `" ++ show r ++ "` has unrealizable kind"
            ++ ". Please look for and eliminate any kind conflicts that arise."
          KVBot () -> throw $ "Record `" ++ show r ++ "` has ambiguous kind. "
            ++ "Please relate it to something with a known kind."
          Record m -> do
            -- Go through all the equality classes and generate the resulting
            -- set of values that we care about.
            s <- RecSBV <$> Map.traverseWithKey (makeRecField fm) m
            add r s
            return s
          _ -> throw $ "Record `" ++ show r ++ "` has a non Record kind of `"
            ++ show kind ++ "`, this should be impossible."
    where
      rname = unpack r
      makeRecField :: Map String (Ref Value)
                   -> String -> EqClassID -> SBVMonad ValueSBV
      makeRecField fm fname' fec
        | Just vi <- Map.lookup fname' fm = do
          -- Make sure the name of this field is what we expect it to be
          let vname = unpack vi
              rvname = rvname' rname fname'
              nameIncorrect = vname /= rvname
          when nameIncorrect (throw $ "Field \"" ++ fname' ++ "\" in "
            ++ "record `" ++ show r ++ "` should have name \"" ++ rvname
            ++ "\" but instead has name \"" ++ vname ++ "\".")
          -- Make sure the Kind of this field is what we expect ti to be
          vkind <- getValEqClassSBV vi >>= getEqClKindSBV ("Building field \""
            ++ fname' ++ "\" " ++ "in record `" ++ show r ++ "`.")
          fkind <- getEqClKindSBV ("Building field \"" ++ fname' ++ "\" "
            ++ "in record `" ++ show r ++ "`.") fec
          let kindIncorrect = fkind /= vkind
          when kindIncorrect (throw $ "Field \"" ++ fname' ++ "\" in "
            ++ "record `" ++ show r ++ "` should have kind \"" ++ show fkind
            ++ "\" but instead has kind \"" ++ show vkind ++ "\".")
          -- Make sure we've not created the value yet
          exists <- uses @SBVS valueRef (Map.member vi)
          -- Then generate the value as needed
          valFromKind vi fkind
        | otherwise = do
          -- There is no existing reference to this element, so just move on
          -- and get the thing you need.
          let rvname = rvname' rname fname'
              vi = Ref rvname
          fkind <- getEqClKindSBV ("Building field \"" ++ fname' ++ "\" "
            ++ "in record `" ++ show r ++ "`.") fec
          valFromKind vi fkind

  lit :: Record -> SBVMonad RecSBV
  lit r = throw $ "There is no notion of a literal for record : " ++ show r

  refConcrete :: String -> Record -> EDGMonad (Ref Record)
  refConcrete name (Record' m) = do
    sp <- Map.traverseWithKey genVal m
    let kind = Record $ Map.map snd sp
        flds = Map.map fst sp
    ec <- newEqClass
    r <- ref name
    classKind @GS %= Map.insert ec kind
    recInfo @GS %= Map.insert r RecInfo{riFields=flds,riEqClass=ec}
    return r
    where
      genVal :: String -> Value -> EDGMonad (Ref Value, EqClassID)
      genVal fn v = do
        let rvname = rvname' name fn
        v <- refConcrete rvname v
        ec <- getValEqClassEDG v
        return (v,ec)

  refAbstract :: String -> RecCons -> EDGMonad (Ref Record)
  refAbstract n RCTop       = throw $ "Cannot create unsatisfiable record `"
    ++ n ++ "`."
  refAbstract n RCBottom    = ref n
  refAbstract n (RCAmbig m) = do
    sp <- Map.traverseWithKey genVal m
    let kind = Record $ Map.map snd sp
        flds = Map.map fst sp
    ec <- newEqClass
    r <- ref n
    classKind @GS %= Map.insert ec kind
    recInfo @GS %= Map.insert r RecInfo{riFields=flds,riEqClass=ec}
    return r
    where
      genVal :: String -> Ambiguous Value -> EDGMonad (Ref Value, EqClassID)
      genVal fn v = do
        let rvname = rvname' n fn
        v <- refAmbiguous rvname v
        ec <- getValEqClassEDG v
        return (v,ec)

  add :: Ref Record -> RecSBV -> SBVMonad ()
  add r s = do
    val <- uses @SBVS recordRef (Map.member r)
    case val of
      True  -> throw $ "Reference to record `" ++ show r ++ "` already exists."
      False -> recordRef @SBVS %= (Map.insert r s)

  getName :: Ref Record -> String
  getName = unpack

  fixAbstract :: RecCons -> EDGMonad RecCons
  fixAbstract RCAmbig{..} = RCAmbig <$> traverse fixAmbiguous rcMap
  fixAbstract a = return a

instance InvertSBV Record where

  extract :: Modelable a => DecodeState -> a -> (Ref Record) -> Maybe Record
  extract = undefined
    -- Lookup the kind of the record, rebuild each child value,
    -- assemble into new record.

-- | Given two records and a list of
recEq :: Bool -> RecSBV -> RecSBV
      -> SBVMonad (SBV Bool)
recEq def as bs = do
    let lk = Map.keys $ Map.union (rsFields as) (rsFields bs)
    ib <- lit def
    foldM (pairEq True as bs) ib lk

-- TODO :: make sure this provides better error messages.
pairEq :: Bool -> RecSBV -> RecSBV
       -> SBV Bool -> String -> SBVMonad (SBV Bool)
pairEq def as bs pv fn = do
  let ma = rsFields as
      mb = rsFields bs
      mva = Map.lookup fn ma
      mvb = Map.lookup fn mb
  sb <- case (mva,mvb) of
    (  Just ValueSBV{vsKindSBV=ak,vsValSBV=av}
      ,Just ValueSBV{vsKindSBV=bk,vsValSBV=bv})
      -> case (av,bv) of
        (Int ai, Int bi)
          -> return $ ((S..==) ak bk) S.&&& ((S..==) ai bi)
        (Bool ab, Bool bb)
          -> return $ ((S..==) ak bk) S.&&& ((S..==) ab bb)
        (Float af, Float bf)
          -> return $ ((S..==) ak bk) S.&&& ((S..==) af bf)
        (String as, String bs)
          -> return $ ((S..==) ak bk) S.&&& ((S..==) as bs)
        (UID au, UID bu)
          -> return $ ((S..==) ak bk) S.&&& ((S..==) au bu)
        (Record ar, Record br) -> do
          let sak = Map.keysSet . rsFields $ ar
              sbk = Map.keysSet . rsFields $ br
              lk = Set.toList $ Set.union sak sbk
          rb <- recEq def as bs
          return $ ((S..==) ak bk) S.&&& rb
        _ -> throw $ "Record `" ++ show ma ++ "` and `" ++ show mb ++ "` "
          ++ "have some field or nested field \"" ++ fn ++ "\" with a "
          ++ "kind mismatch."
    (Nothing, _) -> throw $ "Record `" ++ show ma ++ "` did not have "
      ++ "expected field " ++ fn ++ "."
    (_, Nothing) -> throw $ "Record `" ++ show mb ++ "` did not have "
      ++ "expected field " ++ fn ++ "."
  return $ sb S.&&& pv

instance EDGEquals Record where

  equalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  equalE a b name = do
    n <- ref name
    assertRecKindEq name a b
    returnAnd n $ do
      aeq <- getRecEqClassSBV a
      beq <- getRecEqClassSBV b
      when (aeq /= beq) (throw $ "Records `" ++ show a ++ "` and `" ++ show b
        ++ "` are not set to the same EqClass, yet are being compared"
        ++ " something went wrong here.")
      k <- getEqClKindSBV name aeq
      case k of
        Record m -> do
          as <- sbv a
          bs <- sbv b
          sb <- recEq True as bs
          ob <- sbv n
          constrain $ ob S..== sb
        _ -> throw $ "Record `" ++ show a ++ "` and `" ++ show b ++ "` have "
          ++ "invalid kind : " ++ show k


    -- make sure that all elements are set equal and the
    -- resulting output variable is the && of all of them.


  unequalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  unequalE = undefined

-- Given a record and a string indexing into that record, get the value that
-- we care about.
--
-- This should allow for recursive indexing into the record, and possibly
-- end up creating the field if needs be.
getField :: Ref Record -> String -> EDGMonad (Ref Value)
getField = undefined
