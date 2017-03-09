
module EDG.Elements.Elem where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Lattice
import Algebra.Constrainable

import Control.Newtype

import Control.Monad.MonadSymbolic

import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit hiding ((.>))

import EDG.PortTypes
import EDG.ElemTypes
import EDG.Library.Types
import EDG.Expression
import EDG.ElemTypes
import EDG.EDGDatatype
import EDG.EDGMonad
import EDG.EDGInstances

import EDG.Elements.Port

import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
  )

instance (NamedMonad (ElemM' a b),ExpContext a) =>
  MonadConstrain (ElemM' a b) (Exp a) where
  constrain a = errContext context $ tell @(ES' a b) mempty{esEConstraints = [a]}
    where
      context = "constrain `" ++ show a ++ "`"

embedModule :: String -> ElemDesc Module ModPort -> EDGMonad (Ref Module)
embedModule = undefined

embedLink :: String -> ElemDesc Link LinkPort -> EDGMonad (Ref Link)
embedLink = undefined

-- embedModPort :: String -> PortDesc ModPort -> EDGMonad (Ref ModPort)
-- embedModPort = embedPort' modulePortInfo transform
--   where
--     transform :: (PortInfo ModPort) -> Exp ModPort -> EDGMonad (Exp EDG)
--     transform pi = convertExpressionM litc (varc pi)
--
--     litc :: Constrained' Value -> EDGMonad (Ambiguous Value)
--     litc = return . Abstract . Constrained
--
--     varc :: PortInfo ModPort -> PortValue ModPort -> EDGMonad (Ref Value)
--     varc pi@PortInfo{..} pv
--       | PVUID <- pv = return piPUidRef
--       | PVConnected <- pv = return piPConnected
--       | PVClass <- pv = return piPClass
--       | PVConnectedTo <- pv = return piPConnectedTo
--       | PVType fs <- pv = errContext (context ++ " `" ++ show fs ++ "`") $ do
--           getValL piPType fs
--       where
--         context = "var c `" ++ show pi ++ "`"
--
-- embedLinkPort :: String -> PortDesc LinkPort -> EDGMonad (Ref LinkPort)
-- embedLinkPort = embedPort' linkPortInfo transform
--   where
--     transform :: (PortInfo LinkPort) -> Exp LinkPort -> EDGMonad (Exp EDG)
--     transform pi = convertExpressionM litc (varc pi)
--
--     litc :: Constrained' Value -> EDGMonad (Ambiguous Value)
--     litc = return . Abstract . Constrained
--
--     varc :: PortInfo LinkPort -> PortValue LinkPort -> EDGMonad (Ref Value)
--     varc pi@PortInfo{..} pv
--       | PVUID <- pv = return piPUidRef
--       | PVConnected <- pv = return piPConnected
--       | PVClass <- pv = return piPClass
--       | PVConnectedTo <- pv = return piPConnectedTo
--       | PVType fs <- pv = errContext (context ++ " `" ++ show fs ++ "`") $ do
--           getValL piPType fs
--       where
--         context = "var c `" ++ show pi ++ "`"

embedElem :: forall a b. (ExpContext a,ExpContext b
          , ExpValue a ~ ElemValue a b, ExpLiteral a ~ Constrained' Value
          , ExpValue b ~ PortValue b  , ExpLiteral b ~ Constrained' Value)
          -- The port embedding function
          -- => (String -> PortDesc b -> EDGMonad (Ref b))
          -- lens for adding this sort of elem
          => Lens' GS (Map (Ref a) (ElemInfo a b))
          -- lens we're using for ports and shit (needs to match
          -> Lens' GS (Map (Ref b) (PortInfo b))
          -> String -- name
          -> ElemDesc a b -- desc
          -> EDGMonad (Ref a) -- monad
embedElem elemLens portLens n ed@ElemDesc{..} = do
  uid <- newConcreteUID
  errContext (context uid) $ do
    let name = n ++ "[" ++ show (unpack uid) ++ "]"
        r = Ref name
    -- var for clas
    eclass <- refConcrete (name ++ ".class") . Value . String $ edEClass
    -- type
    etype <- refAmbiguous (name ++ ".type") . transformAmbig Value Constrained
      . transformAmbig Record Record $ edEType
    -- uid
    euidref <- refConcrete (name ++ ".UIDRef") . Value . UID $ uid
    -- used
    eused <- refAbstract (name ++ ".used") . Constrained . Bool $ bottom
    -- constrained
    econstrained <- refAbstract (name ++ ".constrained") . Constrained . Bool
      $ bottom
    -- ports
    let embedPortWName n = embedPort (name ++ ".port." ++ n)
    eports <- Map.traverseWithKey embedPortWName edEPorts
    -- resources
    let errorNum = Lit . Concrete . Value . UID . pack $ -1
    eresources <- Map.fromAscList
      <$> flip mapM (Set.toList edEResources) (\ res -> do
          let rname = name ++ ".resource." ++ unpack r
              usedName = rname ++ ".used"
              userName = rname ++ ".user"
          rused <- refAbstract usedName . Constrained . Bool $ bottom
          ruser <- refAbstract userName . Constrained . UID $ bottom
          -- If unused the UID should be -1
          constrain $ (Not . Val $ rused)
            :=> ((Val ruser :: Exp EDG) :== errorNum)
          -- Key : resource
          -- Value : (Ref Val,Ref Val)
          return (res,(rused,ruser))
        )
    -- convert the resource
    let elemInfo = ElemInfo{
        eiEDesc = ed
      , eiEUID = uid
      , eiEIdent = name
      , eiEClass = eclass
      , eiEType = etype
      , eiEUIDRef = euidref
      , eiEUsed = eused
      , eiEConstrained = econstrained
      , eiEResources = eresources
      , eiEPorts = eports
      , eiEConstraints = mempty
      , eiEResourceCons = mempty
      }
    undefined
    return r
  where
    context uid = "embedElem `" ++ n ++ "` `" ++ show uid ++ "` `"
      ++ show ed ++ "`"

    transformExp :: (ElemInfo a b) -> Exp a -> EDGMonad (Exp EDG)
    transformExp ei = convertExpressionM litc (varc ei)

    litc :: Constrained' Value -> EDGMonad (Ambiguous Value)
    litc = return . Abstract . Constrained

    varc :: ElemInfo a b -> ElemValue a b -> EDGMonad (Ref Value)
    varc ei@ElemInfo{..} ev
      | EVUID <- ev = return eiEUIDRef
      | EVClass <- ev = return eiEClass
      | EVResourceUsed r <- ev = errContext context $ do
        case Map.lookup r eiEResources of
          Nothing -> throw $ "No resource with name `" ++ show r ++ "` found"
            ++ " in elem `" ++ show eiEDesc ++ "`"
          Just (ru,_) -> return ru
      | EVType fs <- ev = errContext (context ++ " `" ++ show fs ++ "`") $ do
        getValL eiEType fs
      | EVPort pn pv <- ev = errContext (context ++ " `" ++ show (pn,pv)
        ++ "`") $ do
          pr <- case Map.lookup pn eiEPorts of
            Nothing -> throw $ "No port named `" ++ pn ++ "` was found within"
              ++ " this element."
          mpi <- uses @GS portLens (Map.lookup pr)
          pi <- case mpi of
            Nothing -> throw $ "No port named `" ++ pn ++ "` with ID `"
              ++ show pr ++ "` was found."
            Just pi -> return pi
          varcP pi pv
      where
        context = "(varc :: ElemInfo) `" ++ show pi ++ "`"

    varcP :: PortInfo b -> PortValue b -> EDGMonad (Ref Value)
    varcP pi@PortInfo{..} pv
      | PVUID <- pv = return piPUidRef
      | PVConnected <- pv = return piPConnected
      | PVClass <- pv = return piPClass
      | PVConnectedTo <- pv = return piPConnectedTo
      | PVType fs <- pv = errContext (context ++ " `" ++ show fs ++ "`") $ do
          getValL piPType fs
      where
        context = "varcP `" ++ show pi ++ "`"

    embedPort :: String -> PortDesc b -> EDGMonad (Ref b)
    embedPort = embedPort' portLens (\ pi -> convertExpressionM litc (varcP pi))



-- -- | The more generic port embedding function, that allows you to add
-- --   to a portInfo given a lens and stuff.
-- embedPort' :: (ExpContext c)
--            => Lens' GS (Map (Ref a) (PortInfo b))
--            -> (PortInfo b -> Exp c -> EDGMonad (Exp EDG))
--            -> String -> PortDesc c -> EDGMonad (Ref a)
-- embedPort' mapLens transformCons n pd@PortDesc{..} = do
--   uid <- newConcreteUID
--   errContext (context uid) $ do
--     let name = n ++ "[" ++ show (unpack uid) ++ "]"
--         r = Ref name
--     -- Build all of the internal values inside the PortInfo
--     pclass  <- refConcrete (name ++ ".class") . Value . String $ pdPClass
--     ptype   <- refAmbiguous (name ++ ".type") . transformAmbig Value Constrained
--                  . transformAmbig Record Record $ pdPType
--     puidref <- refConcrete (name ++ ".UIDRef") . Value . UID $ uid
--     pconnected <- refAbstract (name ++ ".conn") . Constrained . Bool $ bottom
--     pconnectedto <- refAbstract (name ++ ".connTo") . Constrained . UID $ bottom
--     pused <- refAbstract (name ++ ".used") . Constrained . Bool $ bottom
--     -- let pconnections = Map.empty
--     pconstrained <- refAbstract (name ++ ".constrained") . Constrained . Bool $
--       bottom
--     -- build the PortInfo
--     let portInfo = PortInfo{
--         -- Temporary values to allow portINfo to be showed.
--         piPDesc = PortDesc{pdPIdent=pdPIdent,pdPClass=pdPClass,pdPType=pdPType,
--           pdPConstraints=[]}
--       , piPClass = pclass
--       , piPType = ptype
--       , piPUid = uid
--       , piPUidRef = puidref
--       , piPConnected = pconnected
--       , piPConnectedTo = pconnectedto
--       , piPUsed = pused
--       -- , piPConnections = pconnections
--       , piPConstrained = pconstrained
--       , piPConstraints = []
--       }
--     -- And build all the constraints
--     pconstraints <- mapM (transformCons portInfo) pdPConstraints
--     consRefVals <- mapM (\ e -> (show e,) <$> express e) $ pconstraints
--     constrain $ (Val pconstrained :: Exp EDG)
--         :=> (All $ map (Val . snd) consRefVals)
--       -- TODO :: Figure out why you need the explicit type annotations.
--     constrain $ (Val pused :: Exp EDG) :=> (Val pconstrained)
--     constrain $ (Val pconnected :: Exp EDG) :=> (Val pused)
--     -- Make sure the UID set is set to a consistent error value when
--     -- the system isn't connected
--     let errorNum = Lit . Concrete . Value . UID . pack $ -1
--     constrain $ (Not (Val pconnected :: Exp EDG)) :=>
--         ((Val pconnectedto) :== errorNum)
--     -- Build the new portDesc
--     let pd' = PortDesc{pdPIdent=pdPIdent,pdPClass=pdPClass,pdPType=pdPType,
--       pdPConstraints=pconstraints}
--     -- Build the annotated list of constraints
--     -- Add the PortINfo to the storage map
--     mapLens %= Map.insert r portInfo{piPDesc=pd',piPConstraints=consRefVals}
--     return r
--   where
--     context uid = "embedPort `" ++ show n ++ "` `" ++ show uid ++ "` `"
--       ++ show pd ++ "`"

getModuleInfo :: Ref Module -> EDGMonad (ElemInfo Module ModPort)
getModuleInfo = undefined

getLinkInfo :: Ref Link -> EDGMonad (ElemInfo Link LinkPort)
getLinkInfo = undefined

getModulePorts :: Ref Module -> EDGMonad [Ref ModPort]
getModulePorts = undefined

getLinkPorts :: Ref Link -> EDGMonad [Ref LinkPort]
getLinkPorts = undefined

assertModuleUsed :: Ref Module -> EDGMonad ()
assertModuleUsed = undefined

assertLinkUsed :: Ref Link -> EDGMonad ()
assertLinkUsed = undefined

extractModule :: Modelable a => DecodeState -> a -> Ref Module
              -> Maybe (UID', ElemOut Module ModPort)
extractModule = undefined

extractLink :: Modelable a => DecodeState -> a -> Ref Link
              -> Maybe (UID', ElemOut Link LinkPort)
extractLink = undefined

getAllModulePorts :: EDGMonad [Ref ModPort]
getAllModulePorts = undefined

getAllLinkPorts :: EDGMonad [Ref LinkPort]
getAllLinkPorts = undefined
