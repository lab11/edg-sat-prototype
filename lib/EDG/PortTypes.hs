
-- | Here's we we keep the basic semantics of a port, as well as the stuff
--   that lets us convert from the monoid version to a representation we
--   can actually handle.
module EDG.PortTypes where

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


-- | The Port Monad, which lets us be a bit more interesting about how
--   we specify ports and their properties.
--
--   The monad instance on PortState does all the actual work, which is
--   why we're mostly just using the monad instance to shuffle very
--   specific types of data around.
type PortM a = WriterT (PortState a) (Except String)

runPortM :: PortM a () -> PortDesc a
runPortM pm = case runExcept (convertPortState =<< execWriterT pm) of
  Left s -> error $ "portDesc ceration failed with `" ++ s ++ "`"
  Right pd -> pd

getPortMState :: (MonadExcept String m,NamedMonad m)
              => PortM a () -> m (PortState a)
getPortMState p = case runExcept (execWriterT p) of
  Left s -> throw $ "portDesc Creation failed with `" ++ s ++ "`"
  Right ps -> return ps

instance NamedMonad (Except a) where
  monadName = return "Except "

instance NamedMonad (PortM a) where
  monadName = return "Port   "


-- | The state of the port monad, which we'll use to collect pieces of
--   information about the port as we build it.
data PortState a = PortState {
    -- | The name of the port if we have it, this is just a human
    --   readable ID, it doesn't have to be unique or anything.
    psPIdent :: Maybe String
    -- | The class of a port, no port can be connected to a port with a
    --   different class. This limits the number of total possible
    --   connections without overly constraining other things.
  , psPClass :: Maybe String
    -- | This is the constraint on the type signature of the port.
    --   when we add to it the system we make sure that it's viable.
  , psPType :: RecCons
    -- | The set of constraints which must all be true for the port
    --   to be usable.
  , psPConstraints :: [Exp a]
  --   -- | The set of resources the port has available to it.
  -- , psResources :: Set (Resources a)
  --   -- | Expressions with the resources that are used when those
  --   --   expressions are true.
  -- , psResourceConstraints :: Map (PortExp a) (ResMap a)
  }

deriving instance (ExpContext a) => Eq (PortState a)
-- deriving instance (ExpContext a) => Ord (PortState a)
deriving instance (ExpContext a) => Show (PortState a)
deriving instance (ExpContext a) => Read (PortState a)

data PortValue a
  = PVUID
  | PVConnected
  | PVClass
  | PVConnectedTo
  | PVType [String]
  deriving (Eq, Ord, Show, Read)

type PS = PortState

-- | Append a new bit to the port identity
setIdent :: forall a. String -> PortM a ()
setIdent s = tell @(PS a) mempty{psPIdent=Just s}

-- | The UID as usabe in an expression
dUID :: Exp Port
dUID = Val PVUID

-- | is the port connec
dConnected :: Exp Port
dConnected = Val PVConnected

setClass :: forall a. String ->  PortM a (Exp Port)
setClass s = do
  tell @(PS a) mempty{psPClass=Just s}
  return $ Val PVClass

dClass :: Exp Port
dClass = Val PVClass

dConnectedTo :: Exp Port
dConnectedTo = Val PVConnectedTo

setType :: forall a. RecordCons Value -> PortM a (Exp Port)
setType cons = do
  tell @(PS a) mempty{psPType = cons}
  return $ Val (PVType [])

dType :: String -> Exp Port
dType s = Val $ PVType (split '.' s)

addLiteral :: Constrained' Value -> PortM a (Exp Port)
addLiteral = return . Lit

-- | Right this just stores information relative to the port itself.
--   So the assumption that everything that is connected to
instance ExpContext Port where
  type ExpValue   Port = PortValue Port
  type ExpLiteral Port = Constrained' Value

-- | This is how we're representing adding data to a port in order to get
--   more useful output. It nicely synergizes with the structure of a
instance Monoid (PortState a) where
  mempty = PortState {
      psPIdent = Nothing
    , psPClass = Nothing
    , psPType = RCBottom
    , psPConstraints = mempty
    }
  mappend (PortState i  c  t  cons )
          (PortState i' c' t' cons')
      = PortState{
          -- We don't want to lose information, so we try to append
          -- both the new and old idents, with a '~' in between.
          -- This is mainly for human readable shit, so it's fine.
          psPIdent = ((\ i i' -> i ++ "~" ++ i') <$> i <*> i') <|> i' <|> i
        , psPClass = c' <|> c
        , psPType = recordMerge t t'
        , psPConstraints = cons <> cons'
        }

-- | Datatype for a description of a port, what is used as input to
--   the problem description
data PortDesc a = PortDesc {
    pdPIdent :: String
  , pdPClass :: String
  , pdPType :: Ambiguous Record
  , pdPConstraints :: [Exp a]
  }

deriving instance (ExpContext a) => Eq   (PortDesc a)
-- deriving instance (ExpContext a) => Ord  (PortDesc a)
deriving instance (ExpContext a) => Show (PortDesc a)
deriving instance (ExpContext a) => Read (PortDesc a)

convertPortState :: (MonadExcept String m,NamedMonad m)
                 => PortState a -> m (PortDesc a)
convertPortState PortState{..}
  = PortDesc <$> pIdent <*> pClass <*> (Abstract <$> pType) <*> return psPConstraints
  where
    pIdent = case psPIdent of
      Nothing -> throw @String $ "This port has no ident defined, cannot "
        ++ "proceed."
      Just n -> return n
    pClass = case psPClass of
      Nothing -> throw @String $ "This port has no class defined, cannot "
        ++ "proceed."
      Just n -> return n
    pType = case psPType of
      RCTop -> throw @String $ "The type of this port is unsatisfiable."
      o -> return o

-- | The interior datatype of a port, which keep track of all the little
--   references that are relevant, and makes sure we can get them back.
--
--   NOTE :: Important facts that must be true
--    -- If we are being used, constraints must be satisfied.
--    pUsed => pConstrained
--    -- If we are connected we must be being used
--    pConnected => pUsed
--    -- For every potential link we must only be connected to it
--    -- if we are connected to something and the connection is
--    -- NOTE :: Huh, it looks like we can make these assertions
--    --         incrementally since they only require elements
--    --         that are added to the map when a potential link
--    --         is created. There's probably a better way to put it
--    --         in that context though.
--    forall u in (Map.keys piConnections)
--      (fst $ Map.lookup u piConnections)
--        == (pConnected && (pConnectedTo == u))
data PortInfo n = PortInfo {
    piPDesc :: PortDesc EDG -- Original Data for the Port
  , piPClass :: Ref Value -- String -- Our class
  , piPType :: Ref Value -- Record -- Our type
  , piPUid :: UID' -- Our UID
  -- Our UID as represented in the SMT problem
  , piPUidRef :: Ref Value -- UID'
  , piPConnected :: Ref Value -- Bool -- Are we connected to something?
  , piPConnectedTo :: Ref Value -- UID' -- UID of port we're connected to.
  , piPConstrained :: Ref Value -- Bool -- Are our constraints satisfied
  -- The set of actual constraints we have along with their induvidual
  -- references.
  , piPConstraints :: [(String,Ref Value)] -- [(String,Bool)]
  , piPUsed :: Ref Value -- Bool -- Are we being used in the design?
    -- What potential connections? and what Port are they pointing to?
    -- typed so that they'll only ever point to their `opposite` kind
    -- NOTE :: Probably not actually needed since we can make things
    --         aggregate nicely without it, even preserving recoverability..
  -- , piPConnections :: Map UID' (Ref Bool, Ref n)
  }

deriving instance Ord UID'
deriving instance ExpContext EDG => Eq   (PortInfo n)
deriving instance ExpContext EDG => Show (PortInfo n)
deriving instance ExpContext EDG => Read (PortInfo n)

-- | The output type of a port, what we can extract from the finished
--   sat solver output.
data PortOut a = PortOut {
    poPName :: String
  , poPClass :: String
  , poPType :: Record
  , poPConnected :: Bool
  , poPUID :: UID'
  , poPConnectedTo :: Maybe (UID')
  , poPUsed :: Bool
  , poPConstrained :: Bool
  , poPConstraints :: Map String Bool
  }

deriving instance Eq   (PortOut a)
deriving instance Show (PortOut a)
deriving instance Read (PortOut a)

makeLensesWith abbreviatedFields ''PortState
makeLensesWith abbreviatedFields ''PortDesc
makeLensesWith abbreviatedFields ''PortInfo
makeLensesWith abbreviatedFields ''PortOut