
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
import EDG.EDGMonad
import EDG.EDGInstances.Bool
import EDG.SBVWrap
import EDG.EDGDatatype

import EDG.EDGInstances.Bool
import EDG.EDGInstances.String
import EDG.EDGInstances.Float
import EDG.EDGInstances.UID
import EDG.EDGInstances.Integer

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

-- | Get the equality class from the ref to a record
getRecEqClassEDG :: Ref Record -> EDGMonad EqClassID
getRecEqClassEDG a = do
  meca <- uses @GS recInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No Equality class found for Record `" ++ show a ++ "`."
    Just (RecInfo{..}) -> return riEqClass

-- | Get the equality class from the ref to a record
getRecEqClassSBV :: Ref Record -> SBVMonad EqClassID
getRecEqClassSBV a = do
  meca <- uses @SBVS recInfo (Map.lookup a)
  case meca of
    Nothing -> throw $ "No Equality class found for Record `" ++ show a ++ "`."
    Just (RecInfo{..}) -> return riEqClass

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
  valInfo @GS %= (Map.map updateVIMap)
  return new
  where
    -- | Function used to update the ValInfo map with new eqclasses
    updateVIMap e@ValInfo{..}
      = if old == viEqClass then e{viEqClass = new} else e

    -- | Function used to update the Reconfo Map with new eqClasses

-- Mark that the kinds of these two values are equal, unifiying them as
-- neccesary.
assertValKindEq :: String -> Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq context a b = do
  -- | Get the equality classes of both refs
  eca <- getValEqClassEDG a
  ecb <- getValEqClassEDG b
  if | eca == ecb -> return ()
     | otherwise  -> do
          assertEQCKindEq context eca ecb
          return ()
  where
    -- | Given two equality classes, assert that they are equal.
    assertEQCKindEq context eca ecb = do
      ka <- getEqClKindEDG (show a) eca
      kb <- getEqClKindEDG (show b) ecb
      case (ka,kb) of
        (KVTop (),_) -> throw $ "Value `" ++ show a ++ "` has kind `" ++ show ka ++
          "` when trying to unify kinds in context \"" ++ context ++ "\"."
        (_,KVTop ()) -> throw $ "Value `" ++ show b ++ "` has kind `" ++ show kb ++
          "` when trying to unify kinds in context \"" ++ context ++ "\"."
        (KVBot  (), KVBot  ()) -> replaceEqClass eca ecb
        (KVBot  (), kb       ) -> replaceEqClass eca ecb
        (ka       , KVBot  ()) -> replaceEqClass ecb eca -- flipped
        (Int    (), Int    ()) -> replaceEqClass eca ecb
        (Bool   (), Bool   ()) -> replaceEqClass eca ecb
        (Float  (), Float  ()) -> replaceEqClass eca ecb
        (String (), String ()) -> replaceEqClass eca ecb
        (UID    (), UID    ()) -> replaceEqClass eca ecb
        -- TODO :: Make sure this asserts that each field has an identical
        --         kind join the kinds as needed, and whatnot.
        --
        --         get fields that are only in one record
        --         get fields in both records
        --           assert that all these fields have the same kind
        --           gather up their new kinds as needed
        --         ressemble the greater kind for the full field,
        --         add that to the classKind and then ensure both records
        --           and any that share their kind are using that new kind.
        (Record ma, Record mb) -> undefined
        _ -> throwKindErr context a b ka kb
    throwKindErr context a b ka kb = throw $ "Attempting to look up current "
      ++ "kind of " ++
      "\n`" ++ show a ++ "` returned kind `" ++ show ka ++ "` and " ++
      "\n`" ++ show b ++ "` returned kind `" ++ show kb ++ "`" ++
      "\nin context \"" ++ context ++ "\" while trying to assert kind equality."

-- | Set the kind of the value to the given one, as needed
assertValKind :: String -> Ref Value -> Kind' EqClassID -> EDGMonad ()
assertValKind context ref kind = do
  -- | Get the equality classes of both refs
  ec <- getValEqClassEDG ref
  k <- getEqClKindEDG (show ref) ec
  case (k,kind) of
    (KVTop (),_) -> throw $ "Value `"++ show ref ++"` has unsatisfiable kind."
      ++ " Cannot set new kind."
    (KVBot  (),a        ) -> classKind @GS %= (Map.insert ec a)
    (Int    (),Int    ()) -> return ()
    (Bool   (),Bool   ()) -> return ()
    (Float  (),Float  ()) -> return ()
    (String (),String ()) -> return ()
    (UID    (),UID    ()) -> return ()
    (Record ma,Record mb) -> undefined
    _ -> throw $ "Kind of Value `"++show ref++"` is `"++show k++"` and could "
      ++ "not be set to `" ++ show kind ++ "`."

-- | Creates a value from a kind, adds it to the list of things.
valFromKind :: Ref Value -> Kind' EqClassID -> SBVMonad ValueSBV
valFromKind r k =
  case k of
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
    Record rm -> undefined
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
      add r vs
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
    | Record r <- v = undefined
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
          let vref = mkRef . vname' $ n
          -- Generate the reference we'll use to store the concrete value
          u <- newUID
          v <- refConcrete (n ++ ".temp[" ++ show u ++ "]") lit
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
    | Record r <- v = undefined
    -- I love Void. Because it's an uninhabited type, using that value in your
    | KVTop a <- v = throw $ "Cannot create Value `" ++ show n ++ "` as it"
        ++ " is unrealizable."
    | KVBot a <- v = ref n
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
          let vref = mkRef . vname' $ n
          -- Generate the reference we'll use to store the concrete value
          u <- newUID
          v <- refAbstract (n ++ ".temp[" ++ show u ++ "]") lit
          -- Constrain their equality
          isEq <- vref .== v
          constrain isEq
          return r

  add :: Ref Value -> ValueSBV -> SBVMonad ()
  add r s = do
    val <- uses @SBVS valueRef (Map.member r)
    case val of
      True  -> throw $ "Reference to value `" ++ show r ++ "` already exists."
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
        -> Bool -> Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
allEqOp op def a b name = do
  n <- ref name
  assertValKindEq name a b
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
      (Record aeq, Record beq) -> undefined
      -- TODO :: Add error catching for KVTop and KVBot. Those should never
      --         ever get this far, but you know how it goes.
      _ -> do nv <- sbv n
              lv <- lit def
              constrain $ nv S..== lv

instance EDGEquals Value where

  equalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  equalE = allEqOp (S..==) False

  unequalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  unequalE = allEqOp (S../=) True

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
    returnAnd oref (sbvNoDup "Record" recordRef oref)

  sbv :: Ref Record -> SBVMonad RecSBV
  sbv r = undefined
    -- val <- uses @SBVS recordRef (Map.lookup r)
    -- case val of
    --   Just v -> return v
    --   Nothing -> do
    --     -- | See whether we have an equality class for this record
    --     eqcl <- getRecEqClassSBV r
    --     -- | If we do, check the kind
    --     kind <- getEqClKindSBV ("Looking up kind to create Record `" ++ show r
    --       ++ "`.") eqcl
    --     case kind of
    --       KVTop () -> throw $ "Record `" ++ show r ++ "` has unrealizable kind"
    --         ++ ". Please look for and eliminate any kind conflicts that arise."
    --       KVBot () -> throw $ "Record `" ++ show r ++ "` has ambiguous kind. "
    --         ++ "Please relate it to something with a known kind."
    --       Record m -> RecSBV <$> Map.traverseWithKey makeRecField m
    --       _ -> throw $ "Record `" ++ show r ++ "` has a non Record kind of `"
    --         ++ show kind ++ "`, this should be impossible."
    -- where
    --   rname = unpack r
    --   makeRecField fname' fkind = do
    --     let rvname = rvname' rname fname'
    --         rv = Ref rvname
    --     exists <- uses @SBVS valueRef (Map.member rv)
    --     when exists (throw $ "Record field `" ++ rv ++ "` already exists.")
    --     valFromKind rv fkind

  lit :: Record -> SBVMonad RecSBV
  lit r = throw $ "There is no notion of a literal for record : " ++ show r

  refConcrete :: String -> Record -> EDGMonad (Ref Record)
  refConcrete = undefined

  refAbstract :: String -> RecCons -> EDGMonad (Ref Record)
  refAbstract = undefined


  add :: Ref Record -> RecSBV -> SBVMonad ()
  add = undefined

  getName :: Ref Record -> String
  getName = unpack

  fixAbstract :: RecCons -> EDGMonad RecCons
  fixAbstract = undefined

instance InvertSBV Record where

  extract :: Modelable a => DecodeState -> a -> (Ref Record) -> Maybe Record
  extract = undefined

instance EDGEquals Record where

  equalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  equalE = undefined

  unequalE :: Ref Record -> Ref Record -> String -> EDGMonad (Ref Bool)
  unequalE = undefined
