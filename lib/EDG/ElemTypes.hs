
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

import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit

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
type ElemM' t p = WriterT (ElemState t p) (Except String)
type ElemM  t   = ElemM' t (Portify t)

runElemM :: ElemM' t (Portify t) () -> ElemDesc t (Portify t)
runElemM em = case runExcept (convertElemState =<< execWriterT em) of
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
  type ExpLiteral Module = Constrained' Value

instance ExpContext ModPort where
  type ExpValue   ModPort = PortValue ModPort
  type ExpLiteral ModPort = Constrained' Value

-- | So this is about how information is stored relative to a link
instance ExpContext Link where
  type ExpValue   Link = ElemValue Link LinkPort
  type ExpLiteral Link = Constrained' Value

instance ExpContext LinkPort where
  type ExpValue   LinkPort = PortValue LinkPort
  type ExpLiteral LinkPort = Constrained' Value

eNewResource :: forall a. String -> ElemM a (Resource a)
eNewResource (pack -> r) = do
  tell @(ES a) mempty{esEResources=Set.singleton r}
  return r

eSetIdent :: forall a. String -> ElemM a ()
eSetIdent s = tell @(ES a) mempty{esEIdent=Just s}

mUID :: (ExpContext Module, ExpValue Module ~ ElemValue Module ModPort)
     => Exp Module
mUID = Val (EVUID :: ElemValue Module ModPort)

lUID :: (ExpContext Link, ExpValue Link ~ ElemValue Link LinkPort)
     => Exp Link
lUID = Val (EVUID :: ElemValue Link LinkPort)

eSetClass :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
          => String -> ElemM a (Exp a)
eSetClass s = do
  tell @(ES a) mempty{esEClass=Just s}
  return $ Val (EVClass :: ElemValue a b)

mClass :: Exp Module
mClass = Val EVClass

lClass :: Exp Link
lClass = Val EVClass

resourceUsed :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
             => Resource a -> Exp a
resourceUsed r = Val $ (EVResourceUsed r :: ElemValue a b)

portVal :: forall a b. (ExpContext a, ExpValue a ~ ElemValue a b)
        => PortName -> PortValue b -> Exp a
portVal n v = Val $ (EVPort n v :: ElemValue a b)

newPort :: forall a p. (p ~ Portify a, NamedMonad (ElemM' a p))
        => String -> PortM p () -> ElemM' a p (PortName)
newPort s p = do
  p' <- getPortMState p -- :: PortState p
  tell @(ES a) mempty{esEPorts=Map.singleton s p'}
  return s

newResCons :: forall a.  String -> Exp a -> ResourceMap a -> ElemM a ()
newResCons name exp rmap = tell @(ES a) mempty{
    esEResourceCons = Map.singleton name ResourceCons{
        rcPredicate=exp
      , rcUsageMap=rmap
      }
  }

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

convertElemState :: (MonadExcept String m,NamedMonad m, b ~ Portify a)
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
      Nothing -> throw @String $ "This element has no class, cannot "
        ++ "proceed."
      Just v -> return v
    t = return $ Abstract esEType
    con = return esEConstraints
    res = return esEResources
    resCon = return esEResourceCons
    prts = mapM convertPortState esEPorts

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
  , eiEResources   :: Map (Resource n) (Ref Value, Ref Value) -- Bool, UID
  -- | The Resource Constraint name -> (Bool for expression, UIDs assigned for
  --   relevant tags)
  , eiEResourceCons :: Map ResConName (Ref Value, Map ResourceTag (Ref Value))
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
  , eoEPorts :: Map PortName (PortOut b)
  -- | Was used in design?
  , eoEUsed :: Bool
  -- | All constraints satisfied?
  , eoEConstrained :: Bool
  -- | List of all standard constraints and their values for debugging.
  , eoEConstraints :: Map String Bool
  -- | Map of all resources to a possible resourceConstraint that
  --   uses them.
  , eoEResources :: Map (Resource a) (Maybe ResConName)
  -- | The map od all resource constraints, and if fulfilled the resources
  --   and tags that it uses.
  , eoEResourceCons :: Map ResConName (Maybe (Map ResourceTag (Resource a)))
  }

deriving instance () => Eq   (ElemOut a b)
deriving instance () => Show (ElemOut a b)
deriving instance () => Read (ElemOut a b)


makeLensesWith abbreviatedFields ''ResourceCons
makeLensesWith abbreviatedFields ''ElemState
makeLensesWith abbreviatedFields ''ElemDesc
makeLensesWith abbreviatedFields ''ElemInfo
makeLensesWith abbreviatedFields ''ElemOut