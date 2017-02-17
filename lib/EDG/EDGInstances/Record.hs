
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

-- Mark that the kinds of these two values are equal, unifiying them as
-- neccesary.
--
-- TODO :: This is pretty inelegant, I should probably just use a cheap
--         MaybeT to do this.
assertValKindEq :: String -> Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq context a b = do
  meca <- uses @GS valInfo (Map.lookup a)
  mecb <- uses @GS valInfo (Map.lookup b)
  (ValInfo eca _ _ _, ValInfo ecb _ _ _) <- case (meca,mecb) of
    (Just a,Just b) -> return (a,b)
    _ -> throw $ "Attempting to look up current kind of " ++
      "`" ++ show a ++ "` returned eqClass `" ++ show meca ++ "` and " ++
      "`" ++ show b ++ "` returned eqClass `" ++ show mecb ++ "` and " ++
      "in context \"" ++ context ++ "\" while trying to assert kind equality."
  if | eca == ecb -> return ()
     | otherwise  -> do
        mka <- uses @GS classKind (Map.lookup eca)
        mkb <- uses @GS classKind (Map.lookup ecb)
        (ka,kb) <- case (mka,mkb) of
          (Just a,Just b) -> return (a,b)
          _ -> throw $ "Attempting to look up current kind of " ++
            "`" ++ show a ++ "` returned kind `" ++ show mka ++ "` and " ++
            "`" ++ show b ++ "` returned kind `" ++ show mkb ++ "` and " ++
            "in context \"" ++ context ++ "\" while trying to assert kind equality."
        case (ka,kb) of
          (KVTop (),_) -> throw $ "Value `" ++ show a ++ "` has kind `" ++ show ka ++
            "` when trying to unify kinds in context \"" ++ context ++ "\"."
          (_,KVTop ()) -> throw $ "Value `" ++ show b ++ "` has kind `" ++ show kb ++
            "` when trying to unify kinds in context \"" ++ context ++ "\"."
          (KVBot (), kb) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (ka, KVBot ()) -> do
            valInfo @GS %= (Map.map $ updateEC ecb eca)
          (Int (), Int ()) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (Bool (), Bool ()) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (Float (), Float ()) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (String (), String ()) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (UID (), UID ()) -> do
            valInfo @GS %= (Map.map $ updateEC eca ecb)
          (Record ma, Record mb) -> undefined
          _ -> throw $ "Attempting to look up current kind of " ++
            "`" ++ show a ++ "` returned kind `" ++ show mka ++ "` and " ++
            "`" ++ show b ++ "` returned kind `" ++ show mkb ++ "` and " ++
            "in context \"" ++ context ++ "\" while trying to assert kind equality."
  where
    updateEC eca ecb e@ValInfo{..} = if eca == viEqClass then e{viEqClass = ecb} else e

instance SBVAble Value where

  type SBVType Value = ValueSBV

  type RefType Value = Ref Value

  ref :: String -> EDGMonad (Ref Value)
  ref name = throw $ "Cannot create a bare value reference : " ++ name

  lit :: Value     -> SBVMonad ValueSBV
  lit v = error "There is no notion of a literal for values."

  refConcrete :: String -> Value -> EDGMonad (Ref Value)
  refConcrete n (unpack -> v)
    | Int    i <- v = refSingleton Int    Int    (Int    ()) i v
    | Bool   b <- v = refSingleton Bool   Bool   (Bool   ()) b v
    | Float  f <- v = refSingleton Float  Float  (Float  ()) f v
    | String s <- v = refSingleton String String (String ()) s v
    | UID    u <- v = refSingleton UID    UID    (UID    ()) u v
    | Record r <- v = undefined
    -- I love Void. Because it's an uninhabited type, using that value in your
    -- code consitutes a proof that that portion of your code is unreachable.
    | KVTop a <- v = absurd a
    | KVBot a <- v = absurd a
      where
        -- We have to duplicat parameters a bit here, since the type
        -- system can't handle complex polymorphism like the constructors.
        -- Multiple instances here just specialize to the correct type easily.
        refSingleton :: (SBVAble f)
                     => (RefType f -> ValRef)
                     -> (SBVType f -> ValSBV)
                     -> Kind' EqClassID
                     -> f
                     -> Value' Value
                     -> EDGMonad (Ref Value)
        refSingleton c1 c2 kind i v = do
          let oref  = Ref n
              orig  = Concrete . pack $ v
              vname = n ++ ".data"
              kname = n ++ ".kind"
              knum  = getKindNum v
          vref <- refConcrete vname i
          kref <- refConcrete kname knum
          eqcl <- newEqClass
          let vinfo = ValInfo{viEqClass  = eqcl
                             ,viOriginal = orig
                             ,viValRef   = c1 vref
                             ,viKindRef  = kref
                             }
          exists <- uses @GS valInfo $ Map.member oref
          when exists (throw $ "Value `" ++ show oref ++ "` already exists.")
          valInfo   @GS %= Map.insert oref vinfo
          classKind @GS %= Map.insert eqcl kind
          returnAnd oref $ do
            kv <- sbv kref
            vv <- sbv vref
            add oref ValueSBV{vsKindSBV = kv, vsValSBV = c2 vv}

  refAbstract :: String -> Constrained -> EDGMonad (Ref Value)
  refAbstract n (unpack -> v)
    | Int    i <- v = refSingleton Int    Int    (Int    ()) i v
    | Bool   b <- v = refSingleton Bool   Bool   (Bool   ()) b v
    | Float  f <- v = refSingleton Float  Float  (Float  ()) f v
    | String s <- v = refSingleton String String (String ()) s v
    | UID    u <- v = refSingleton UID    UID    (UID    ()) u v
    | Record r <- v = undefined
    | KVTop a <- v = throw $ "Attempted to create a ref for unstaisfiable value `"
                     ++ n ++ "`. Make sure every value is satisfiable."
    | KVBot a <- v = throw $ "Attempted to create a ref for value `" ++ n
                     ++ "` with no kind specified. Make sure every value/field has a kind."
      where
        -- We have to duplicat parameters a bit here, since the type
        -- system can't handle complex polymorphism like the constructors.
        -- Multiple instances here just specialize to the correct type easily.
        refSingleton :: (SBVAble f)
                     => (RefType f -> ValRef)
                     -> (SBVType f -> ValSBV)
                     -> Kind' EqClassID
                     -> Constraints f
                     -> Constrained' Value
                     -> EDGMonad (Ref Value)
        refSingleton c1 c2 kind i v = do
          let oref  = Ref n
              orig  = Abstract . pack $ v
              vname = n ++ ".data"
              kname = n ++ ".kind"
              knum  = getKindNum v
          vref <- refAbstract vname i
          kref <- refConcrete kname knum
          eqcl <- newEqClass
          let vinfo = ValInfo{viEqClass  = eqcl
                             ,viOriginal = orig
                             ,viValRef   = c1 vref
                             ,viKindRef  = kref
                             }
          exists <- uses @GS valInfo $ Map.member oref
          when exists (throw $ "Value `" ++ show oref ++ "` already exists.")
          valInfo   @GS %= Map.insert oref vinfo
          classKind @GS %= Map.insert eqcl kind
          returnAnd oref $ do
            kv <- sbv kref
            vv <- sbv vref
            add oref ValueSBV{vsKindSBV = kv, vsValSBV = c2 vv}

  sbv :: Ref Value -> SBVMonad ValueSBV
  sbv r = do
    val <- uses @SBVS valueRef (Map.lookup r)
    case val of
      Nothing -> throw $ "No ref to value `" ++ show r ++ "` found, cannot continue."
      Just v  -> return v


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
        kname = name ++ ".kind"
        dname = name ++ ".data"
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
