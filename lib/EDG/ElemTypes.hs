
-- | Types for emodules and links in the design,
module EDG.ElemTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Constrainable

import Control.Newtype
import Control.Newtype.Util

import Control.Lens.TH

import Control.Applicative ((<|>)) -- Alternative
import Data.Monoid ((<>)) -- Mappend

import EDG.Expression
import EDG.EDGDatatype

import Control.Monad.Trans.Class
-- import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit
import Control.Monad.Ether.Implicit.Writer
import Control.Monad.Ether.Implicit.Reader
import Control.Monad.Ether.Implicit.Except
import Control.Monad.Ether.Implicit.State.Strict
import Control.Lens.TH

import EDG.Library.Types
import EDG.PortTypes

-- | Tag that determines how a particular resource can be used in a given
--   environment.
type ResourceTag = String

type ResourceMap a = Map ResourceTag (Set (Resource a))

-- | A resource constraint, which given some predicate, requires that
--   a set of resources are used when that predicate is true.
data ResourceCons a = ResourceCons {
    -- The actual expression that will consume resources when true
    rcPredicate :: Exp a
    -- For each tag, at least one of the resources given must be available
    -- and assigned.
  , rcUsageMap :: ResourceMap a
  }

deriving instance (ExpContext a) => Eq   (ResourceCons a)
deriving instance (ExpContext a) => Show (ResourceCons a)
deriving instance (ExpContext a) => Read (ResourceCons a)

-- | The Element monad, which we use to let people more easily assemble
--   complex monads.
type ElemM' t p = StateT Integer (WriterT (ElemState t p) (Except String))
type ElemM  t   = ElemM' t (Portify t)

runElemM :: (ExpContext (Portify t))
          => ElemM' t (Portify t) () -> ElemDesc t (Portify t)
runElemM em = case runExcept (convertElemState
  =<< execWriterT (evalStateT em 1)) of
  Left s -> error $ "ElemDesc creation failed with `" ++ s ++ "`"
  Right ed -> ed

instance NamedMonad (ElemM' Module ModPort) where
  monadName = return "Module "

instance NamedMonad (ElemM' Link LinkPort) where
  monadName = return "Link   "

-- | The state of the element that forms a monoid within writer we
--   will use to capture information about an element as we build it up
data ElemState a b = ElemState {
    -- The human readable identifier we use for describing a partially
    -- converted element.
    esEIdent :: Maybe String
    -- The class of the element
  , esEClass :: Maybe String
    -- The type of the element.
  , esEType :: RecCons
    -- The set of constraints over the element
  , esEConstraints :: [Exp a]
    -- The set of available resources
  , esEResources :: Set (Resource a)
    -- Constraints that involve the use of resources
  , esEResourceCons :: Map ResConName (ResourceCons a)
    -- The set of ports in the design and the names they're using.
  , esEPorts :: Map PortName (PortState b)
  }

instance Monoid (ElemState a b) where
  mempty = ElemState{
      esEIdent = Nothing
    , esEClass = Nothing
    , esEType = RCBottom
    , esEConstraints = mempty
    , esEResources = mempty
    , esEResourceCons = mempty
    , esEPorts = mempty
    }
  mappend (ElemState i  c  t  cons  res  resCons  ps )
          (ElemState i' c' t' cons' res' resCons' ps')
      = ElemState{
          esEIdent = ((\ i i' -> i ++ "~" ++ i') <$> i <*> i') <|> i' <|> i
        , esEClass = c' <|> c
        , esEType = recordMerge t t'
        , esEConstraints = cons <> cons'
        , esEResources = res <> res'
        , esEResourceCons = resCons' <> resCons
        , esEPorts = Map.unionWith (<>) ps ps'
        }

deriving instance (ExpContext a, ExpContext b) => Eq   (ElemState a b)
deriving instance (ExpContext a, ExpContext b) => Show (ElemState a b)
deriving instance (ExpContext a, ExpContext b) => Read (ElemState a b)

-- | The type of somthing that indexes into the the type of an element
type TypeIndex = [String]
type PortName = String

data ElemValue a b
  = EVUID
  | EVType TypeIndex
  -- As boolean of whether resource is being used?
  | EVResourceUsed (Resource a)
  | EVClass
  | EVPort PortName (PortValue b)

-- | So this is about how information is stored relative to a module
instance ExpContext Module where
  type ExpValue   Module = ElemValue Module ModPort
  type ExpLiteral Module = Ambiguous Value

instance ExpContext ModPort where
  type ExpValue   ModPort = PortValue ModPort
  type ExpLiteral ModPort = Ambiguous Value

-- | So this is about how information is stored relative to a link
instance ExpContext Link where
  type ExpValue   Link = ElemValue Link LinkPort
  type ExpLiteral Link = Ambiguous Value

instance ExpContext LinkPort where
  type ExpValue   LinkPort = PortValue LinkPort
  type ExpLiteral LinkPort = Ambiguous Value

evNewInt :: ElemM a Integer
evNewInt = do
  newInt <- get
  put (newInt + 1)
  return newInt

evNewResource :: forall a. String -> ElemM a (Resource a)
evNewResource (pack -> r) = do
  tell @(ES a) mempty{esEResources=Set.singleton r}
  return r

evType :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
       => String -> Exp a
evType s = Val $ EVType (split '.' s)

evSetIdent :: forall a. String -> ElemM a ()
evSetIdent s = tell @(ES a) mempty{esEIdent=Just s}

evUID :: forall e p. (ExpContext e
     , ExpValue e ~ ElemValue e p)
     => Exp e
evUID = Val (EVUID :: ElemValue e p)

evSetClass :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
          => String -> ElemM a (Exp a)
evSetClass s = do
  tell @(ES a) mempty{esEClass=Just s}
  return $ Val (EVClass :: ElemValue a b)

evClass :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b) => Exp a
evClass = Val EVClass

evResourceUsed :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
             => Resource a -> Exp a
evResourceUsed r = Val $ (EVResourceUsed r :: ElemValue a b)

evPortVal :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
        => PortName -> PortValue b -> Exp a
evPortVal n v = Val $ (EVPort n v :: ElemValue a b)

evNewPort :: forall a p. (p ~ Portify a, NamedMonad (ElemM' a p))
        => String -> PortM p () -> ElemM' a p (PortName)
evNewPort s p = do
  p' <- getPortMState p -- :: PortState p
  tell @(ES a) mempty{esEPorts=Map.singleton s p'}
  return s

evNewResCons :: forall a.  String -> Exp a -> ResourceMap a -> ElemM a ()
evNewResCons name exp rmap = tell @(ES a) mempty{
    esEResourceCons = Map.singleton name ResourceCons{
        rcPredicate=exp
      , rcUsageMap=rmap
      }
  }

evSetType :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
          => RecordCons Value -> ElemM a (Exp a)
evSetType cons = do
  tell @(ES a) mempty{esEType = cons}
  return $ Val (EVType [])

deriving instance () => Eq   (ElemValue a b)
deriving instance () => Show (ElemValue a b)
deriving instance () => Read (ElemValue a b)

type ES' a b = ElemState a b
type ES  a   = ES' a (Portify a)

-- | Names for resource constraints.
type ResConName = String

-- | A more concrete description of an element, instead of the partial one
--   that a PortState is.
data ElemDesc a b = ElemDesc {
    -- The identifier we use as a human readable name
    edEIdent :: String
    -- The class of the port, this is mostly just so there's an easily
    -- acessible Idenifier or something.
  , edEClass :: String
    -- The type of the port
  , edEType :: Ambiguous Record
    -- The set of constraints over the element
  , edEConstraints :: [Exp a]
    -- The set of available resources
  , edEResources :: Set (Resource a)
    -- Constraints that involve the use of resources
  , edEResourceCons :: Map ResConName (ResourceCons a)
    -- The set of ports in the design and the names they're using.
  , edEPorts :: Map PortName (PortDesc b)
  }

deriving instance (ExpContext a, ExpContext b) => Eq   (ElemDesc a b)
deriving instance (ExpContext a, ExpContext b) => Show (ElemDesc a b)
deriving instance (ExpContext a, ExpContext b) => Read (ElemDesc a b)

convertElemState :: (ExpContext b,
                    MonadExcept String m,NamedMonad m, b ~ Portify a)
                 => ElemState a b -> m (ElemDesc a b)
convertElemState ElemState{..}
  = ElemDesc <$> i <*> c <*> t <*> con
    <*> res <*> resCon <*> prts
  where
    i = case esEIdent of
      Nothing -> throw @String $ "This element has no identifier, cannot "
        ++ "proceed."
      Just v -> return v
    c = case esEClass of
      Nothing -> throw @String $ "This element has no signature, cannot "
        ++ "proceed."
      Just v -> return v
    t = return $ Abstract esEType
    con = return esEConstraints
    res = return esEResources
    resCon = return esEResourceCons
    prts = mapM convertPortState esEPorts

data ResourceInfo = ResourceInfo {
    riUsed :: Ref Value
  , riUid  :: Ref Value
  , riUser :: Ref Value
  }

deriving instance () => Eq   (ResourceInfo)
deriving instance () => Show (ResourceInfo)
deriving instance () => Read (ResourceInfo)

data ResourceTagInfo = ResourceTagInfo {
    rtiUsed  :: Ref Value
  , rtiUid   :: Ref Value
  , rtiUsing :: Ref Value
  }

deriving instance () => Eq   (ResourceTagInfo)
deriving instance () => Show (ResourceTagInfo)
deriving instance () => Read (ResourceTagInfo)

-- | The interior datatype of an element which keeps track of all the
--   references that are relevant, and ensures they're retrievable for
--   later use.
data ElemInfo n p = ElemInfo {
    eiEDesc :: ElemDesc n p
  , eiEUID   :: UID'
  , eiEIdent :: String
  , eiEClass :: Ref Value -- String
  , eiEType  :: Ref Value -- Record
  , eiEUIDRef   :: Ref Value -- UID'
  , eiEPorts :: Map PortName (Ref p)
  , eiEUsed  :: Ref Value -- Bool
  , eiEConstrained :: Ref Value -- Bool
  , eiEConstraints :: Map String (Ref Value {-Bool-})
  -- | Whether the resource is used and what the UID is if it is used.
  , eiEResources   :: Map (Resource n) ResourceInfo -- Bool, UID
  -- | The Resource Constraint name ->
  --   (Bool for expression, Bool for resCons being met,
  --    UIDs assigned for relevant tags)
  , eiEResourceCons :: Map ResConName (Ref Value,
      Map ResourceTag ResourceTagInfo)
  }

deriving instance (ExpContext n,ExpContext p) => Eq   (ElemInfo n p)
deriving instance (ExpContext n,ExpContext p) => Show (ElemInfo n p)
deriving instance (ExpContext n,ExpContext p) => Read (ElemInfo n p)

-- | The output type of an element, what we end up extracting from the
--   finished SATSolver output.
data ElemOut a b = ElemOut {
  -- | The Identifier
    eoEIdent :: String
  -- | The class of element
  , eoEClass :: String
  -- | The type of the element
  , eoEType :: Record
  -- | The UID of the element
  , eoEUID :: UID'
  -- | Each port in the element
  , eoEPorts :: Map PortName (UID', PortOut b)
  -- | Was used in design?
  , eoEUsed :: Bool
  -- | All constraints satisfied?
  , eoEConstrained :: Bool
  -- | List of all standard constraints and their values for debugging.
  , eoEConstraints :: Map String Bool
  -- | Map of all resources to a possible resourceConstraint that
  --   uses them.
  , eoEResources :: Map (Resource a) ResourceOut
  -- | The map od all resource constraints, and if fulfilled the resources
  --   and tags that it uses.
  , eoEResourceCons :: Map ResConName (Bool,
    Maybe (Map ResourceTag ResourceTagOut))
  }

deriving instance () => Eq   (ElemOut a b)
deriving instance () => Show (ElemOut a b)
deriving instance () => Read (ElemOut a b)

data ResourceOut = ResourceOut{
    roUsed :: Bool
  , roUid :: UID'
  , roUser :: Maybe UID'
  }

deriving instance () => Eq   ResourceOut
deriving instance () => Show ResourceOut
deriving instance () => Read ResourceOut

data ResourceTagOut = ResourceTagOut {
    rtoUsed :: Bool
  , rtoUid :: UID'
  , rtoUsing :: Maybe UID'
  }

deriving instance () => Eq   ResourceTagOut
deriving instance () => Show ResourceTagOut
deriving instance () => Read ResourceTagOut

makeLensesWith abbreviatedFields ''ResourceCons
makeLensesWith abbreviatedFields ''ResourceInfo
makeLensesWith abbreviatedFields ''ResourceTagInfo
makeLensesWith abbreviatedFields ''ResourceOut
makeLensesWith abbreviatedFields ''ResourceTagOut
makeLensesWith abbreviatedFields ''ElemState
makeLensesWith abbreviatedFields ''ElemDesc
makeLensesWith abbreviatedFields ''ElemInfo
makeLensesWith abbreviatedFields ''ElemOut
