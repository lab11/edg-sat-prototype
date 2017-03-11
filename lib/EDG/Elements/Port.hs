
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

import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
  )

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

  -- Neccesary
  expressPlus   :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressPlus = (.+)
  expressMinus  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressMinus = (.-)
  expressTimes  :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressTimes = (.*)
  -- expressDiv    :: Ref Value -> Ref Value -> EDGMonad (Ref Value)
  expressNegate :: Ref Value -> EDGMonad (Ref Value)
  expressNegate = negateE'

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
embedPort n pd =
  embedPort' barePortInfo transform n pd
    where
      transform :: (PortInfo Port) -> Exp Port -> EDGMonad (Exp EDG)
      transform pi = convertExpressionM litc (varc pi)

      litc :: Constrained' Value -> EDGMonad (Ambiguous Value)
      litc = return . Abstract . Constrained

      varc :: PortInfo Port -> PortValue Port -> EDGMonad (Ref Value)
      varc pi@PortInfo{..} pv
        | PVUID <- pv = return piPUidRef
        | PVConnected <- pv = return piPConnected
        | PVClass <- pv = return piPClass
        | PVConnectedTo <- pv = return piPConnectedTo
        | PVType fs <- pv = errContext (context ++ " `" ++ show fs ++ "`") $ do
            getValL piPType fs
        where
          context = "var c `" ++ show pi ++ "`"


-- | The more generic port embedding function, that allows you to add
--   to a portInfo given a lens and stuff.
embedPort' :: (ExpContext c)
           => Lens' GS (Map (Ref a) (PortInfo b))
           -> (PortInfo b -> Exp c -> EDGMonad (Exp EDG))
           -> String -> PortDesc c -> EDGMonad (Ref a)
embedPort' mapLens transformCons n pd@PortDesc{..} = do
  uid <- newConcreteUID
  errContext (context uid) $ do
    let name = n ++ "[" ++ show (unpack uid) ++ "]"
        r = Ref name
    -- Build all of the internal values inside the PortInfo
    pclass  <- refConcrete (name ++ ".class") . Value . String $ pdPClass
    ptype   <- refAmbiguous (name ++ ".type") . transformAmbig Value Constrained
                 . transformAmbig Record Record $ pdPType
    puidref <- refConcrete (name ++ ".UIDRef") . Value . UID $ uid
    pconnected <- refAbstract (name ++ ".conn") . Constrained . Bool $ bottom
    pconnectedto <- refAbstract (name ++ ".connTo") . Constrained . UID $ bottom
    pused <- refAbstract (name ++ ".used") . Constrained . Bool $ bottom
    -- let pconnections = Map.empty
    pconstrained <- refAbstract (name ++ ".constrained") . Constrained . Bool $
      bottom
    -- build the PortInfo
    let portInfo = PortInfo{
        -- Temporary values to allow portINfo to be showed.
        piPDesc = PortDesc{pdPIdent=pdPIdent,pdPClass=pdPClass,pdPType=pdPType,
          pdPConstraints=[]}
      , piPClass = pclass
      , piPType = ptype
      , piPUid = uid
      , piPUidRef = puidref
      , piPConnected = pconnected
      , piPConnectedTo = pconnectedto
      , piPUsed = pused
      -- , piPConnections = pconnections
      , piPConstrained = pconstrained
      , piPConstraints = []
      }
    -- And build all the constraints
    pconstraints <- mapM (transformCons portInfo) pdPConstraints
    consRefVals <- mapM (\ e -> (show e,) <$> express e) $ pconstraints
    constrain $ (Val pconstrained :: Exp EDG)
        :=> (All $ map (Val . snd) consRefVals)
      -- TODO :: Figure out why you need the explicit type annotations.
    constrain $ (Val pused :: Exp EDG) :=> (Val pconstrained)
    constrain $ (Val pconnected :: Exp EDG) :=> (Val pused)
    -- Make sure the UID set is set to a consistent error value when
    -- the system isn't connected
    let errorNum = Lit . Concrete . Value . UID . pack $ -1
    constrain $ (Not (Val pconnected :: Exp EDG)) :=>
        ((Val pconnectedto) :== errorNum)
    -- Build the new portDesc
    let pd' = PortDesc{pdPIdent=pdPIdent,pdPClass=pdPClass,pdPType=pdPType,
      pdPConstraints=pconstraints}
    -- Build the annotated list of constraints
    -- Add the PortINfo to the storage map
    mapLens %= Map.insert r portInfo{piPDesc=pd',piPConstraints=consRefVals}
    return r
  where
    context uid = "embedPort `" ++ show n ++ "` `" ++ show uid ++ "` `"
      ++ show pd ++ "`"

-- | Gets you the portinfo variable for the port itself.
getPortInfo :: Ref Port -> EDGMonad (PortInfo Port)
getPortInfo = getPortInfo' "BarePortInfo" barePortInfo

getPortInfo' :: String -> Lens' GS (Map (Ref a) (PortInfo b))
             -> Ref a -> EDGMonad (PortInfo b)
getPortInfo' errName portMap r = errContext context $ do
    mpi <- uses @GS portMap (Map.lookup r)
    case mpi of
      Nothing -> throw $ "No Port `" ++ show r ++ "` found in " ++ errName
        ++ "."
      Just pi -> return pi
  where
    context = "(getPortInfo :: " ++ errName ++ ") '" ++ show r ++ "`"

-- | Given two ports, sets up the infrastructure to ensure they are connected
--   this is actually pretty nontrivial :/
--
--   TODO :: convert this to a nicely lensed version like the others.
areBarePortsConnected :: Ref Port-> Ref Port -> EDGMonad (Ref Value)
areBarePortsConnected = arePortsConnected barePortInfo barePortInfo
  "barePortInfo" "barePortInfo"

areElemPortsConnected :: Ref LinkPort -> Ref ModPort -> EDGMonad (Ref Value)
areElemPortsConnected = arePortsConnected linkPortInfo modulePortInfo
  "linkPortInfo" "modulePortInfo"

arePortsConnected :: ()
                  => Lens' GS (Map (Ref a) (PortInfo a))
                  -> Lens' GS (Map (Ref (Flip a)) (PortInfo (Flip a)))
                  -> String
                  -> String
                  -> Ref a -> Ref (Flip a) -> EDGMonad (Ref Value)
arePortsConnected lens lens' s s' p p' = errContext context $ do
  -- retieve the portInfo data
  p1 <- getPortInfo' s  lens  p
  p2 <- getPortInfo' s' lens' p'
  -- Create the output variable and
  let oname = "connected? (" ++ show p ++ ") (" ++ show p' ++ ")"
  o <- ref oname
  vo <- bootstrapValue (Bool o)
  let whenConn = Val vo :: Exp EDG
  -- Make sure that classes are equal when the things are connected
  constrain $ whenConn :=> ((p1 <!> pClass) :== (p2 <!> pClass))
  -- Make sure that types are equal when the things are connected
  constrain $  whenConn :=> ((p1 <!> pType) :== (p2 <!> pType))
  -- both ports are being used if this connection is true
  constrain $ whenConn :=> (p1 <!> pUsed)
  constrain $ whenConn :=> (p2 <!> pUsed)
  -- They are both connected if this connection is true
  constrain $ whenConn :=> (p1 <!> pConnected)
  constrain $ whenConn :=> (p2 <!> pConnected)
  -- And their ConnectedTo is equal to the UID of the other iff connected.
  --
  -- NOTE :: This constraint and the fact that UIDs are unique should enforce
  --         that connnections are one-to-one, *and* that connections are
  --         mutual.
  --
  --         For fucks sake don't mess with this unless you know exactly
  --         what you're going. >:|
  constrain $ whenConn :== ((p1 <!> pConnectedTo) :== (p2 <!> pUidRef))
  constrain $ whenConn :== ((p2 <!> pConnectedTo) :== (p1 <!> pUidRef))
  -- Return the boolean that describes whether the variables are connected
  return vo
  where
    context = "areBarePortsConnected `" ++ show p ++ "` `" ++ show p' ++ "`"

    -- I dunno, i just don't want to write this out 15 million times.
    (<!>) :: PortInfo f -> Lens' (PortInfo f) (Ref Value) -> Exp EDG
    (<!>) pt l = Val $ pt ^. l

-- | Make sure the port will be used in the design
--
--   TODO :: Convert this to a nicely lens parameterized version.
assertPortUsed :: Ref Port -> EDGMonad ()
assertPortUsed p = errContext context $ do
  pi <- getPortInfo p
  constrain (Val $ pi ^. pUsed :: Exp EDG)
  where
    context = "assertPortUsed `" ++ show p ++ "`"

extractPort :: Modelable a => DecodeState -> a -> Ref Port
            -> Maybe (UID',PortOut Port)
extractPort = extractPort' getDSBarePortInfo

extractModPort :: Modelable a => DecodeState -> a -> Ref ModPort
            -> Maybe (UID',PortOut ModPort)
extractModPort = extractPort' getDSModulePortInfo

extractLinkPort :: Modelable a => DecodeState -> a -> Ref LinkPort
            -> Maybe (UID',PortOut LinkPort)
extractLinkPort = extractPort' getDSLinkPortInfo

extractPort' :: Modelable a
             => (DecodeState -> Map (Ref b) (PortInfo b))
             -> DecodeState -> a -> Ref b
             -> Maybe (UID',PortOut b)
extractPort' retfun ds model port = do
  pi <- maybeThrow' ("No portInfo found for `" ++ show port ++ "`") $
    Map.lookup port pim
  let poName = pi ^. pDesc . pIdent
  -- Get the class
  poClass' <- extract ds model (pi ^. pClass)
  poClass <- case poClass' of
    Value (String s) -> return s
    _ -> fail $ "the pClass variable `" ++ show poClass' ++ "` in `" ++ show port ++ "` is not a string"
  -- The type
  poType' <- extract ds model (pi ^. pType)
  poType <- case poType' of
    Value (Record s) -> return s
    _ -> fail $ "the pType variable `" ++ show poType' ++ "`  in `" ++ show port ++ "` is not a record"
  -- Whether the port was connected and whom to
  poConnected' <- extract ds model (pi ^. pConnected)
  poConnected <- case poConnected' of
    Value (Bool s) -> return s
    _ -> fail $ "the pConnected variable `" ++ show poConnected' ++ "` in `" ++ show port ++ "` is not a bool"
  poConnectedTo <- case poConnected of
    False -> return Nothing
    True -> do
      poConnectedTo' <- extract ds model (pi ^. pConnectedTo)
      case poConnectedTo' of
        Value (UID s) -> return (Just s)
        _ -> fail $ "the pConnectedTo variable `" ++ show poConnectedTo' ++ "` in `" ++ show port ++ "`"
              ++ " isn't a UID"
  -- Whether the port is being used
  poUsed' <- extract ds model (pi ^. pUsed)
  poUsed <- case poUsed' of
    Value (Bool s) -> return s
    _ -> fail $ "the pUsed variable `" ++ show poUsed' ++ "` in `" ++ show port ++ "` is not a bool"
  -- Whether the port has all of its constraints met
  poConstrained' <- extract ds model (pi ^. pConstrained)
  poConstrained <- case poConstrained' of
    Value (Bool s) -> return s
    _ -> fail $ "the pConstrained variable `" ++ show poConstrained' ++ "` in `" ++ show port
      ++ "` is not a Bool"
  poConstraints' <- flip mapM (pi ^. pConstraints)
    (\ (s,v) -> (s,) <$> extract ds model v)
  poConstraints <- flip mapM poConstraints' (\ (s,vr) -> case vr of
    Value (Bool b) -> return (s,b)
    _ -> fail $ "the constraint `" ++ s ++ "` in the pConstraints "
      ++ "variable `" ++ show poConstraints' ++ "` is not a bool")
  -- TODO :: add a check to make sure the referenced UID is the same as the
  --         stored UID
  -- Assemble the output
  return (pi ^. pUid,PortOut{
      poPName = poName
    , poPClass = poClass
    , poPType = poType
    , poPUID = pi ^. pUid
    , poPConnected = poConnected
    , poPConnectedTo = poConnectedTo
    , poPUsed = poUsed
    , poPConstrained = poConstrained
    , poPConstraints = Map.fromList poConstraints
    })
  where
    pim = retfun ds
    fail = error

--- Link and module stuff ---

-- embedLinkPort :: String -> PortDesc Link -> EDGMonad (Ref LinkPort)
-- embedLinkPort = undefined

-- embedModPort :: String -> PortDesc Module -> EDGMonad (Ref ModPort)
-- embedModPort = undefined

-- arePortsConnected :: Ref LinkPort -> Ref ModPort -> EDGMonad (Ref Bool)
-- arePortsConnected = undefined

-- portToLinkPort :: Ref Port -> PortDesc Port -> PortDesc LinkPort
-- portToLinkPort = undefined

-- portToModPort :: Ref Port -> PortDesc Port -> PortDesc ModPort
-- portToModPort = undefined

-- extractLinkPort :: Modelable a => DecodeState -> a -> Ref Port -> PortOut Link
-- extractModPort :: Modelable a => DecodeState -> a -> Ref Port -> PortOut Mod
