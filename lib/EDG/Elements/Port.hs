
module EDG.Elements.Port where

import Data.Map (Map)
import qualified Data.Map as Map

import Algebra.Lattice
import Algebra.Constrainable

import Control.Newtype

import Control.Monad.MonadSymbolic

import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit hiding ((.>))

import EDG.PortTypes
import EDG.Library.Types
import EDG.Expression
import EDG.ElemTypes
import EDG.EDGDatatype
import EDG.EDGMonad
import EDG.EDGInstances


-- | No other good place to keep this instance for now.
instance ExpContext a => MonadConstrain (PortM a) (Exp a) where
  constrain s = errContext context $ tell mempty{psPConstraints = [s]}
    where
      context = "constrain `" ++ show s ++ "`"

-- | No other good place to keep this instance for now.
instance (Expressible EDG EDGMonad) => MonadConstrain EDGMonad (Exp EDG) where
  constrain s = errContext context $ constrain =<< express s
    where
      context = "constrain `" ++ show s ++ "`"

instance ExpContext EDG where
  type ExpValue EDG = Ref Value
  type ExpLiteral EDG = Ambiguous Value

instance Expressible EDG EDGMonad where
  type ExpRuntime EDG = Ref Value

  intToLit  :: Integer -> EDGMonad (Ambiguous Value)
  intToLit i = return . Concrete . Value . Int $ i

  boolToLit  :: Bool -> EDGMonad (Ambiguous Value)
  boolToLit b = return $ Concrete . Value . Bool $ b

  expressLit :: Ambiguous Value -> EDGMonad (Ref Value)
  expressLit l = errContext context $ do
    uid <- newUID
    errContext (context ++ " `" ++ show uid ++ "`") $
      refAmbiguous ("Literal[" ++ show uid ++ "]") l
    where
      context = "expressLit `" ++ show l ++ "`"

  expressVal :: Ref Value -> EDGMonad (Ref Value)
  expressVal = return

  -- Equality
  expressEq  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressEq a b = bootstrapValue =<< (Bool <$> (a .== b))
  expressNeq :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressNeq a b = bootstrapValue =<< (Bool <$> (a ./= b))

  --
  expressAnd     :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressAnd = (.&&)
  expressOr      :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressOr = (.||)
  expressNot     :: Ref Value -> EDGMonad (Ref Value)
  expressNot = notE'
  expressNand :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressNand = (.~&)
  expressNor :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressNor = (.~|)
  expressXor :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressXor = (.<+>)
  expressImplies :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressImplies = (.=>)

  expressLT  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressLT a b = bootstrapValue =<< (Bool <$> (a .< b))
  expressLTE :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressLTE a b = bootstrapValue =<< (Bool <$> (a .<= b))
  expressGT  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressGT a b = bootstrapValue =<< (Bool <$> (a .> b))
  expressGTE :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressGTE a b = bootstrapValue =<< (Bool <$> (a .>= b))
--
--   -- Neccesary
--   expressPlus   :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
--   expressMinus  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
--   expressTimes  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
--   expressDiv    :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
--   expressNegate :: Ref Value -> EDGMonad (Ref Value)
--
  -- Neccesary
  expressIf :: Ref Value -> Ref Value -> Ref Value
            -> EDGMonad (Ref Value)
  expressIf c t f = {- errContext context $ -} do
    o <- ref ("if `" ++ getName c ++ "` then `" ++ getName t ++ "` else `"
      ++ getName f ++ "`")
    -- TODO :: There's probably a better way to do this by making it
    --         a concrete operation typeclass in EDG.EDGMonad.
    --         ALso figure out why we need explicit annotations.
    constrain $      (Val c :: Exp EDG)  :=> ((Val t) :== (Val o))
    constrain $ (Not (Val c :: Exp EDG)) :=> ((Val f) :== (Val o))
    return o
    where
      context = "expressIf `" ++ show c ++ "` then `" ++ show t
        ++ "` else `" ++ show f ++ "`"

-- -- | Datatype for a description of a port, what is used as input to
-- --   the problem description
-- data PortDesc a = PortDesc {
--     pdPIdent :: String
--   , pdPClass :: String
--   , pdPType :: Ambiguous Record
--   , pdPConstraints :: [Exp a]
--   }

-- Given a name, and a transformation function actually embed the type
embedPort :: String -> PortDesc Port -> EDGMonad (Ref Port)
embedPort n pd = do
  u <- newConcreteUID
  embedPort' basePortInfo transform n u pd
    where
      transform :: (PortInfo Port) -> Exp Port -> EDGMonad (Exp EDG)
      transform = undefined

-- | The more generic port embedding function, that allows you to add
--   to a portInfo given a lens and stuff.
embedPort' :: (ExpContext c)
           => Lens' GS (Map (Ref a) (PortInfo b))
           -> (PortInfo b -> Exp c -> EDGMonad (Exp EDG))
           -> String -> UID' -> PortDesc c -> EDGMonad (Ref a)
embedPort' mapLens transformCons n uid pd@PortDesc{..}
  = errContext context $ do
    let name = n ++ "[" ++ show (unpack uid) ++ "]"
        r = Ref name
    -- Build all of the internal values inside the PortInfo
    pclass  <- refConcrete (name ++ ".class") . Value . String $ pdPClass
    ptype   <- refAmbiguous (name ++ ".type") . transformAmbig Value Constrained
                 . transformAmbig Record Record $ pdPType
    puidref <- refConcrete (name ++ ".uidRef") . Value . UID $ uid
    pconnected <- refAbstract (name ++ ".conn") . Constrained . Bool $ bottom
    pconnectedto <- refAbstract (name ++ ".connTo") . Constrained . UID $ bottom
    pused <- refAbstract (name ++ ".used") . Constrained . Bool $ bottom
    let pconnections = Map.empty
    pconstrained <- refAbstract (name ++ ".constrained") . Constrained . Bool $
      bottom
    -- build the PortInfo
    let portInfo = PortInfo{
        piPDesc = undefined
      , piPClass = pclass
      , piPType = ptype
      , piPUid = uid
      , piPUidRef = puidref
      , piPConnected = pconnected
      , piPConnectedTo = pconnectedto
      , piPUsed = pused
      , piPConnections = pconnections
      , piPConstrained = pconstrained
      }
    -- And build all the constraints
    pconstraints <- mapM (transformCons portInfo) pdPConstraints
    constrain $ (Val pconstrained) :=> (All pconstraints)
      -- TODO :: Figure out why you need the explicit type annotations.
    constrain $ (Val pused :: Exp EDG) :=> (Val pconstrained)
    constrain $ (Val pconnected :: Exp EDG) :=> (Val pused)
    -- Build the new portDesc
    let pd' = PortDesc{pdPIdent=pdPIdent,pdPClass=pdPClass,pdPType=pdPType,
      pdPConstraints=pconstraints}
    -- Add the PortINfo to the storage map
    mapLens %= Map.insert r portInfo{piPDesc=pd'}
    return r
  where
    context = "embedPort `" ++ show n ++ "` `" ++ show uid ++ "` `"
      ++ show pd ++ "`"

areBarePortsConnected :: Ref Port -> Ref Port -> EDGMonad (Ref Bool)
areBarePortsConnected = undefined

-- extractPort :: Modelable a => DecodeState -> a -> Ref Port -> PortOut Port

--- Link and module stuff ---

embedLinkPort :: String -> PortDesc Link -> EDGMonad (Ref LinkPort)
embedLinkPort = undefined

embedModPort :: String -> PortDesc Module -> EDGMonad (Ref ModPort)
embedModPort = undefined

arePortsConnected :: Ref LinkPort -> Ref ModPort -> EDGMonad (Ref Bool)
arePortsConnected = undefined

portToLinkPort :: Ref Port -> PortDesc Port -> PortDesc LinkPort
portToLinkPort = undefined

portToModPort :: Ref Port -> PortDesc Port -> PortDesc ModPort
portToModPort = undefined

-- extractLinkPort :: Modelable a => DecodeState -> a -> Ref Port -> PortOut Link
-- extractModPort :: Modelable a => DecodeState -> a -> Ref Port -> PortOut Mod
