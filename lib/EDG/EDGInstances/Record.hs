
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
assertValKindEq :: Ref Value -> Ref Value -> EDGMonad ()
assertValKindEq = undefined

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
              vname = n ++ ".val"
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
    -- I love Void. Because it's an uninhabited type, using that value in your
    -- code consitutes a proof that that portion of your code is unreachable.
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
              vname = n ++ ".val"
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
  extract = undefined

instance EDGEquals Value where

  equalE :: Ref Value -> Ref Value -> String -> EDGMonad (Ref Bool)
  equalE a b name = do
    n <- ref name
    assertValKindEq a b
    returnAnd n $ do
      akind <- getValueKind a
      bkind <- getValueKind b
      case (akind,bkind) of
        (Int     (), Int     ()) -> undefined
        (Bool    (), Bool    ()) -> undefined
        (Float   (), Float   ()) -> undefined
        (String  (), String  ()) -> undefined
        (UID     (), UID     ()) -> undefined
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
    assertValKindEq a b
    returnAnd n $ do
      akind <- getValueKind a
      bkind <- getValueKind b
      case (akind,bkind) of
        (Int     (), Int     ()) -> undefined
        (Bool    (), Bool    ()) -> undefined
        (Float   (), Float   ()) -> undefined
        (String  (), String  ()) -> undefined
        (UID     (), UID     ()) -> undefined
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

  notE     :: Ref Value ->              String -> EDGMonad (RefType Bool)
  notE = undefined

  andE     :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  andE = undefined

  orE      :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  orE = undefined

  impliesE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  impliesE = undefined

-- | The EDGOrd instances for Value all assert kind equality and error on
--   kind mismation or use on a kind which doesn't have an EDGLogic instance
instance EDGOrd Value where

  gtE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gtE = undefined

  gteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  gteE = undefined

  ltE  :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  ltE = undefined

  lteE :: Ref Value -> Ref Value -> String -> EDGMonad (RefType Bool)
  lteE = undefined

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
