
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

-- Mark that the kinds of these two values are equal, unifiying them as
-- neccesary.
--
-- TODO :: This is pretty inelegant, I should probably just use a cheap
--         MaybeT to do this.
assertValKindEq :: String -> Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq context a b = do
  -- | Get the equality classes of both refs
  meca <- uses @GS valInfo (Map.lookup a)
  mecb <- uses @GS valInfo (Map.lookup b)
  (ValInfo eca _ _, ValInfo ecb _ _) <- case (meca,mecb) of
    (Just a,Just b) -> return (a,b)
    _ -> throwKindErr context a b meca mecb
  if | eca == ecb -> return ()
     | otherwise  -> do
          assertEQCKindEq context eca ecb
          return ()
  where
    -- | Function used to update the map with new eqclasses
    updateEC eca ecb e@ValInfo{..} = if eca == viEqClass then e{viEqClass = ecb} else e
    -- | Given two equality classes, assert that they are equal.
    assertEQCKindEq context eca ecb = do
      mka <- uses @GS classKind (Map.lookup eca)
      mkb <- uses @GS classKind (Map.lookup ecb)
      (ka,kb) <- case (mka,mkb) of
        (Just a,Just b) -> return (a,b)
        _ -> throwKindErr context a b mka mkb
      case (ka,kb) of
        (KVTop (),_) -> throw $ "Value `" ++ show a ++ "` has kind `" ++ show ka ++
          "` when trying to unify kinds in context \"" ++ context ++ "\"."
        (_,KVTop ()) -> throw $ "Value `" ++ show b ++ "` has kind `" ++ show kb ++
          "` when trying to unify kinds in context \"" ++ context ++ "\"."
        (KVBot (), KVBot ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (KVBot (), kb) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (ka, KVBot ()) -> do
          valInfo @GS %= (Map.map $ updateEC ecb eca) -- This one is flipped on purpose
          return eca
        (Int (), Int ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (Bool (), Bool ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (Float (), Float ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (String (), String ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
        (UID (), UID ()) -> do
          valInfo @GS %= (Map.map $ updateEC eca ecb)
          return ecb
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
        _ -> throwKindErr context a b mka mkb
    throwKindErr context a b mka mkb = throw $ "Attempting to look up current "
      ++ "kind of " ++
      "\n`" ++ show a ++ "` returned kind `" ++ show mka ++ "` and " ++
      "\n`" ++ show b ++ "` returned kind `" ++ show mkb ++ "`" ++
      "\nin context \"" ++ context ++ "\" while trying to assert kind equality."

-- | Set the kind of the value to the given one, as needed
assertValKind :: String -> Ref Value -> Kind' EqClassID -> EDGMonad ()
assertValKind context ref kind = do
  -- | Get the equality classes of both refs
  mvi <- uses @GS valInfo (Map.lookup ref)
  vi <- case mvi of
    Nothing -> throw $ "No Value `" ++ show ref ++ "` exists, so cannot set "
      ++ "its kind."
    Just v -> return v
  let ec = viEqClass vi
  mk <- uses @GS classKind (Map.lookup ec)
  k <- case mk of
    Nothing -> throw $ "Value `" ++ show ref ++ "` has no kind specified."
    Just v -> return v
  case (k,kind) of
    (KVTop (),_) -> throw $ "Value `"++ show ref ++"` has unsatisfiable kind."
      ++ " Cannot set new kind."
    (KVBot (),a) ->
      classKind @GS %= (Map.insert ec a)
    (Int (),Int ()) -> return ()
    (Bool (),Bool ()) -> return ()
    (Float (),Float ()) -> return ()
    (String (),String ()) -> return ()
    (UID (),UID ()) -> return ()
    (Record ma,Record mb) -> undefined
    _ -> throw $ "Kind of Value `"++show ref++"` is `"++show k++"` and could "
      ++ "not be set to `" ++ show kind ++ "`."


-- | Given a name for a Value Reference gives the name for the data reference
vname' = (++ ".data")

-- | Given a name for a Value Reference gives the name for the kind reference
kname' = (++ ".kind")

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
          Nothing -> throw $ "No kind information for Value `" ++ show r ++ "`."
          Just (KVTop ()) -> throw $ "Value `" ++ show r ++ "` has "
            ++ "an unrealizable kind of `KVTop ()`."
          Just (KVBot ()) -> throw $ "Value `" ++ show r ++ "` has "
            ++ "an ambiguous kind and we cannnot choose a specific realization"
            ++ " for it. Ensure that it is set equal to something with a fixed"
            ++ " kind at some point in the build process."
          Just k@(Int    ()) -> sbvSingleton r k Int
          Just k@(Bool   ()) -> sbvSingleton r k Bool
          Just k@(Float  ()) -> sbvSingleton r k Float
          Just k@(String ()) -> sbvSingleton r k String
          Just k@(UID    ()) -> sbvSingleton r k UID
          Just k@(Record fm) -> undefined -- sbvSingleton r k Record
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

  lit :: Value     -> SBVMonad ValueSBV
  lit v = error "There is no notion of a literal for Values."

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


instance EDGEquals Value where

  equalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  equalE a b name = do
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
          constrain $ ov S..== (aiv S..== biv)
        (Bool    (), Bool    ()) -> do
          (ValueSBV _ (Bool aiv)) <- sbv a
          (ValueSBV _ (Bool biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S..== biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S..== biv)
        (String  (), String  ()) -> do
          (ValueSBV _ (String aiv)) <- sbv a
          (ValueSBV _ (String biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S..== biv)
        (UID     (), UID     ()) -> do
          (ValueSBV _ (UID aiv)) <- sbv a
          (ValueSBV _ (UID biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S..== biv)
        (Record aeq, Record beq) -> undefined
        -- TODO :: Add error catching for KVTop and KVBot. Those should never
        --         ever get this far, but you know how it goes.
        --
        -- Kinds don't match, therefore cannot be equal. :V
        _ -> do nv <- sbv n
                lv <- lit False
                constrain $ nv S..== lv

  unequalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  unequalE a b name = do
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
          constrain $ ov S..== (aiv S../= biv)
        (Bool    (), Bool    ()) -> do
          (ValueSBV _ (Bool aiv)) <- sbv a
          (ValueSBV _ (Bool biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S../= biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S../= biv)
        (String  (), String  ()) -> do
          (ValueSBV _ (String aiv)) <- sbv a
          (ValueSBV _ (String biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S../= biv)
        (UID     (), UID     ()) -> do
          (ValueSBV _ (UID aiv)) <- sbv a
          (ValueSBV _ (UID biv)) <- sbv b
          ov <- sbv n
          constrain $ ov S..== (aiv S../= biv)
        (Record aeq, Record beq) -> undefined
        -- TODO :: Add error catching for KVTop and KVBot. Those should never
        --         ever get this far, but you know how it goes.
        --
        -- Kinds don't match, therefore cannot be equal. :V
        _ -> do nv <- sbv n
                lv <- lit True
                constrain $ nv S..== lv

-- | The EDGLogic instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which is doesn't have an EDGLogic instance
instance EDGLogic Value where

  notE :: Ref Value ->  String -> EDGMonad (RefType Value)
  notE r name = do
    n <- refAbstract @Value name (pack $ Bool bottom)
    returnAnd n $ do
      rkind <- getValueKind r
      case rkind of
        Bool{} -> do
          (ValueSBV _ (Bool riv)) <- sbv r
          (ValueSBV _ (Bool niv)) <- sbv n
          constrain $ riv S../= niv
        _ -> throw $ "Could not create `" ++ show n ++ "` because `" ++ show r
          ++ "` does not have kind of Bool."

  andE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  andE a b name = do
    n <- refAbstract @Value name (pack $ Bool bottom)
    returnAnd n $ do
      akind <- getValueKind a
      bkind <- getValueKind b
      case (akind, bkind) of
        (Bool (),Bool ()) -> do
          (ValueSBV _ (Bool aiv)) <- sbv a
          (ValueSBV _ (Bool biv)) <- sbv b
          (ValueSBV _ (Bool niv)) <- sbv n
          constrain $ niv S..== (aiv S.&&& biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             "at least one of which is not Bool."

  orE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  orE a b name = do
    n <- refAbstract @Value name (pack $ Bool bottom)
    returnAnd n $ do
      akind <- getValueKind a
      bkind <- getValueKind b
      case (akind, bkind) of
        (Bool (),Bool ()) -> do
          (ValueSBV _ (Bool aiv)) <- sbv a
          (ValueSBV _ (Bool biv)) <- sbv b
          (ValueSBV _ (Bool niv)) <- sbv n
          constrain $ niv S..== (aiv S.||| biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             "at least one of which is not Bool."

  impliesE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Value)
  impliesE a b name = do
    n <- refAbstract @Value name (pack $ Bool bottom)
    returnAnd n $ do
      akind <- getValueKind a
      bkind <- getValueKind b
      case (akind, bkind) of
        (Bool (),Bool ()) -> do
          (ValueSBV _ (Bool aiv)) <- sbv a
          (ValueSBV _ (Bool biv)) <- sbv b
          (ValueSBV _ (Bool niv)) <- sbv n
          constrain $ niv S..== (aiv S.==> biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             "at least one of which is not Bool."

-- | The EDGOrd instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which doesn't have an EDGLogic instance
instance EDGOrd Value where

  gtE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gtE a b name = do
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
          constrain $ ov S..== (aiv S..> biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv a
          ov <- sbv n
          constrain $ ov S..== (aiv S..> biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             ", both kinds must be identical and numeric."

  gteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gteE a b name = do
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
          constrain $ ov S..== (aiv S..>= biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv a
          ov <- sbv n
          constrain $ ov S..== (aiv S..>= biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             ", both kinds must be identical and numeric."


  ltE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  ltE a b name = do
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
          constrain $ ov S..== (aiv S..< biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv a
          ov <- sbv n
          constrain $ ov S..== (aiv S..< biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             ", both kinds must be identical and numeric."


  lteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  lteE a b name = do
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
          constrain $ ov S..== (aiv S..<= biv)
        (Float   (), Float   ()) -> do
          (ValueSBV _ (Float aiv)) <- sbv a
          (ValueSBV _ (Float biv)) <- sbv a
          ov <- sbv n
          constrain $ ov S..== (aiv S..<= biv)
        _ -> throw $ "Could not create `" ++ show n ++ "` because " ++
             "`" ++ show a ++ "` has kind `" ++ show akind ++ "` and" ++
             "`" ++ show b ++ "` has kind `" ++ show bkind ++ "` and" ++
             ", both kinds must be identical and numeric."


instance SBVAble Record where

  type SBVType Record = RecSBV

  type RefType Record = (Ref Record)

  ref :: String -> EDGMonad (Ref Record)
  ref = undefined

  refConcrete :: String -> Record -> EDGMonad (Ref Record)
  refConcrete = undefined

  refAbstract :: String -> RecCons -> EDGMonad (Ref Record)
  refAbstract = undefined

  sbv :: Ref Record -> SBVMonad RecSBV
  sbv = undefined

  lit :: Record -> SBVMonad RecSBV
  lit = undefined

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
