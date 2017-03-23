
module EDG.Elements.Elem where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import EDG.EDGMonad hiding (trace)
import EDG.EDGInstances

import Debug.Trace

import EDG.Elements.Port

import qualified Data.Text.Lazy as T
import qualified Text.Pretty.Simple as P

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
embedModule = embedElem moduleInfo modulePortInfo

embedLink :: String -> ElemDesc Link LinkPort -> EDGMonad (Ref Link)
embedLink = embedElem linkInfo linkPortInfo

-- | Embeds an element into the EDG Monad, eventually turning it into a
--   a set of SMT constraints.
--
--   TODO :: Clear this up, move all the traverses into their own functions.
embedElem :: forall a b. (ExpContext a,ExpContext b
          , ExpValue a ~ ElemValue a b, ExpLiteral a ~ Ambiguous Value
          , ExpValue b ~ PortValue b  , ExpLiteral b ~ Ambiguous Value)
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
    let name = n ++ "[#" ++ show (unpack uid) ++ "#]"
        r = Ref name
    -- var for clas
    eclass <- refConcrete (name ++ ".class") . Value . String $ edEClass
    -- var for type
    etype <- refAmbiguous (name ++ ".type") . transformAmbig Value Constrained
      . transformAmbig Record Record $ edEType
    -- var for uid
    euidref <- refConcrete (name ++ ".UIDRef") . Value . UID $ uid
    -- var for is used
    eused <- refAbstract (name ++ ".used") . Constrained . Bool $ bottom
    -- var that implies all constraints
    econstrained <- refAbstract (name ++ ".constrained") . Constrained . Bool
      $ bottom
    -- embed all the ports
    let embedPortWName n = embedPort (name ++ ".port." ++ n)
    eports <- Map.traverseWithKey embedPortWName edEPorts
    -- Resources
    let errorNum = Lit . Concrete . Value . UID . pack $ -1
    eresources <- Map.fromAscList
      <$> flip mapM (Set.toList edEResources) (\ res -> do
          -- For each resource
          let rname = name ++ ".resource." ++ unpack res
              usedName = rname ++ ".used"
              uidName  = rname ++ ".uid"
              userName = rname ++ ".user"
          -- Create variables for whether the resource is used and who the
          -- user is.
          rused <- refAbstract usedName . Constrained . Bool $ bottom
          ruidnum <- newConcreteUID
          ruid <- refConcrete uidName . Value . UID $ ruidnum
          ruser <- refAbstract userName . Constrained . UID $ bottom
          -- If unused the UID should be -1
          constrain $ ((Not . Val $ rused) :&& (Val econstrained))
           :=> ((Val ruser :: Exp EDG) :== errorNum)
          -- Key : resource
          -- Value : (Ref Val,Ref Val)
          return (res,ResourceInfo{
                riUsed=rused
              , riUid=ruid
              , riUser=ruser
            })
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
    -- And build all the constraints
    econstraints' <- mapM (transformExp elemInfo) edEConstraints
    consRefVals  <- mapM (\ e -> (show e,) <$> express e) $ econstraints'
    constrain $ (Val econstrained :: Exp EDG)
        :=> (All $ map (Val . snd) consRefVals)
    let econstraints = Map.fromAscList consRefVals
    -- And the resource constraints
    eresourceconsAndUseMaps <- flip Map.traverseWithKey edEResourceCons
      -- for each resource constraint
      (\ rcn ResourceCons{..} -> do
        -- name for the resourcecons variables
        let rcname = name ++ ".resCons." ++ rcn
        -- get the variable for the expression these resources are
        -- predicated on.
        rcexp <- express =<< transformExp elemInfo rcPredicate
        -- get the map of specific tagUIDs
        rctagsAndUseMaps <- flip Map.traverseWithKey rcUsageMap
          -- For each tag
          (\ tn (Set.toList -> rs) -> do
            let tname = rcname ++ ".tag." ++ tn
            -- get a unique new ID
            tuidnum <- newConcreteUID
            tused <- refAbstract (tname ++ ".used")
                . Constrained . Bool $ bottom
            tuid <- refConcrete (tname ++ ".uid") . Value . UID $ tuidnum
            tusing <- refAbstract (tname ++ ".using")
                . Constrained . UID $ bottom
            -- If unused the using flag should be -1 otherwise it should
            -- be something else.
            constrain $ (Val econstrained) :=> ((Not . Val $ tused)
              :== ((Val tusing :: Exp EDG) :== errorNum))
            -- If the parent expression is true and the element is
            -- being used then this tag must use a resource/
            constrain ((Val rcexp :&& Val econstrained)
              :=> Val tused :: Exp EDG)
            -- for each possibility get the constraint.
            (tagEqVals :: [Exp EDG],resUseMaps
              :: [Map (Ref Value,Ref Value) [Ref Value]]) <- unzip <$>
              flip mapM rs (\ r -> do
              case Map.lookup r eresources of
                Nothing -> throw $ "No resource `" ++ show r ++ "` found "
                  ++ "when generating constraints for tag `" ++ tn ++ "`"
                  ++ " in resource constraint `" ++ rcn ++ "`"
                -- If we're using this, both the tag and the resource are
                -- being used, the tag's UID is the resources user, and the
                -- resource UID is what the tag is using.
                Just ResourceInfo{..} -> return (All[
                    Val tuid :== Val riUser
                  , Val riUid :== Val tusing
                  , Val tused
                  , Val riUsed
                  ] :: Exp EDG, Map.singleton (riUser,riUsed) [tuid])
              )
            -- There must be a matching tag if the expression is true and the
            -- system is being constrained.
            constrain $ ((Val rcexp) :&& (Val econstrained))
              :=> (Any tagEqVals)
            return (ResourceTagInfo{
                rtiUsed=tused
              , rtiUid=tuid
              , rtiUsing=tusing
              },resUseMaps)
          )
        let rctags = Map.map fst rctagsAndUseMaps
            recUseMaps = concat . map snd . Map.elems $ rctagsAndUseMaps
        return ((rcexp,rctags),recUseMaps)
      )
    -- Now that we've (tediously) gotten the backwards mapping of resources
    -- to the set of tags that can use them, we constrain the resources so
    -- that they are *only* usable by those tags.
    let eresourcecons = Map.map fst eresourceconsAndUseMaps
        recUseMap = Map.unionsWith (++) . concat . map snd . Map.elems
          $ eresourceconsAndUseMaps
    flip Map.traverseWithKey recUseMap (\ (ruser,rused) lusers -> do
      let terms = map (\ l -> (Val ruser :: Exp EDG) :== (Val l)) lusers
      constrain $ ((Val econstrained) :&& (Val rused) :: Exp EDG)
        :=> (Any terms)
      )
    -- go through the use map and constrain the shit out of each tag
    -- Basic important constraints applicable to all elements.
    constrain $ (Val eused :: Exp EDG) :=> (Val econstrained)
    -- Make sure all the ports' used flags are equal to ours
    flip Map.traverseWithKey eports (\ name ref -> errContext ("Tying port `"
      ++ name ++ "` with ref `" ++ show ref ++ "` to parent elem with "
      ++ "implicit constraints.") $ do
        mpi <- uses @GS portLens (Map.lookup ref)
        pi <- case mpi of
          Nothing -> throw $ "No port `" ++ show ref ++ "` found, cannot "
            ++ "generate constraints."
          Just pi -> return pi
        -- This port equals value is set equal to the link's
        constrain $ (Val eused :: Exp EDG) :== (Val $ pi ^. pUsed)
      )
    elemLens %= Map.insert r elemInfo{
        eiEConstraints=econstraints,
        eiEResourceCons=eresourcecons
      }
    return r
  where
    context uid = "embedElem `" ++ n ++ "` `" ++ show uid ++ "` `"
      ++ (T.unpack $ P.pShow ed) ++ "`"

    transformExp :: (ElemInfo a b) -> Exp a -> EDGMonad (Exp EDG)
    transformExp ei = convertExpressionM litc (varc ei)

    litc :: Ambiguous Value -> EDGMonad (Ambiguous Value)
    litc = return

    varc :: ElemInfo a b -> ElemValue a b -> EDGMonad (Ref Value)
    varc ei@ElemInfo{..} ev
      | EVUID <- ev = return eiEUIDRef
      | EVClass <- ev = return eiEClass
      | EVResourceUsed r <- ev = errContext context $ do
        case Map.lookup r eiEResources of
          Nothing -> throw $ "No resource with name `" ++ show r ++ "` found"
            ++ " in elem `" ++ show eiEDesc ++ "`"
          Just ResourceInfo{..} -> return riUsed
      | EVType fs <- ev = errContext (context ++ " `" ++ show fs ++ "`") $ do
        getValL eiEType fs
      | EVPort pn pv <- ev = errContext (context ++ " `" ++ show (pn,pv)
        ++ "`") $ do
          -- Lookup the port
          pr <- case Map.lookup pn eiEPorts of
            Nothing -> throw $ "No port named `" ++ pn ++ "` was found within"
              ++ " this element."
            Just pr -> return pr
          mpi <- uses @GS portLens (Map.lookup pr)
          pi <- case mpi of
            Nothing -> throw $ "No port named `" ++ pn ++ "` with ID `"
              ++ show pr ++ "` was found."
            Just pi -> return pi
          -- Get the value inside the port that this was reffering to.
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


getModuleInfo :: Ref Module -> EDGMonad (ElemInfo Module ModPort)
getModuleInfo r = errContext context $ do
  mmi <- uses @GS moduleInfo (Map.lookup r)
  case mmi of
    Nothing -> throw $ "No module `" ++ show r ++ "` found."
    Just mi -> return mi
  where
    context = "getModuleInfo `" ++ show r ++ "`"

getLinkInfo :: Ref Link -> EDGMonad (ElemInfo Link LinkPort)
getLinkInfo r = errContext context $ do
  mli <- uses @GS linkInfo (Map.lookup r)
  case mli of
    Nothing -> throw $ "No link `" ++ show r ++ "` found."
    Just li -> return li
  where
    context = "getLinkInfo `" ++ show r ++ "`"

getModulePorts :: Ref Module -> EDGMonad [Ref ModPort]
getModulePorts r = errContext context$ Map.elems . eiEPorts <$> getModuleInfo r
  where
    context = "getModulePorts `" ++ show r ++ "`"

getLinkPorts :: Ref Link -> EDGMonad [Ref LinkPort]
getLinkPorts r = errContext context$ Map.elems . eiEPorts <$> getLinkInfo r
  where
    context = "getLinkPorts `" ++ show r ++ "`"

assertModuleUsed :: Ref Module -> EDGMonad ()
assertModuleUsed r = errContext context $ do
  mi <- getModuleInfo r
  constrain $ mi ^. eUsed
  where
    context = "assertModuleUsed `" ++ show r ++ "`"

assertLinkUsed :: Ref Link -> EDGMonad ()
assertLinkUsed r = errContext context $ do
  li <- getLinkInfo r
  constrain $ li ^. eUsed
  where
    context = "assertModuleUsed `" ++ show r ++ "`"

getAllModulePorts :: EDGMonad [Ref ModPort]
getAllModulePorts = errContext context $ do
  mrs <- uses @GS moduleInfo Map.keys
  concat <$> mapM getModulePorts mrs
  where
    context = "getAllModulePorts"

getAllLinkPorts :: EDGMonad [Ref LinkPort]
getAllLinkPorts = errContext context $ do
  lrs <- uses @GS linkInfo Map.keys
  concat <$> mapM getLinkPorts lrs
  where
    context = "getAllLinkPorts"

-- | Operations to be run at the end of the SMT generation phase
--   since that's the only time we have all the neccesary data.
finishUpConstraints :: EDGMonad ()
finishUpConstraints = do
  mapM_ constrainModPortConn =<< getAllModulePorts
  mapM_ constrainLinkPortConn =<< getAllLinkPorts

createOptionalConnection :: Ref LinkPort -> Ref ModPort
                         -> EDGMonad (Maybe (Ref Value))
createOptionalConnection rl rm = errContext context $ do
  mlpi <- uses @GS linkPortInfo (Map.lookup rl)
  lpi <- case mlpi of
    Nothing -> throw $ "Could not find linkPort with name `" ++ show rl ++ "`"
    Just lpi -> return lpi
  mmpi <- uses @GS modulePortInfo (Map.lookup rm)
  mpi <- case mmpi of
    Nothing -> throw $ "Could not find modulePort with name `" ++ show rl ++ "`"
    Just mpi -> return mpi
  if (mpi ^. pDesc . pClass :: String) == (lpi ^. pDesc . pClass :: String)
    then Just <$> areElemPortsConnected rl rm
    else errContext ("port `" ++ show rl ++ "` and `" ++ show rm ++ "` "
        ++ "don't have the same class.") $ return Nothing
  where
    context = "createOptionalConnection `" ++ show rl ++ "` `" ++ show rm ++ "`"

createAllOptionalConnections :: EDGMonad (Map (Ref LinkPort, Ref ModPort)
  (Ref Value))
createAllOptionalConnections = errContext context $ do
  lps <- getAllLinkPorts
  mps <- getAllModulePorts
  let pairs = [(l,m) | l <- lps, m <- mps]
  mmap <- Map.fromList <$> flip mapM pairs (\ p -> do
      mo <- uncurry createOptionalConnection p
      return (p,mo)
    )
  return $ Map.mapMaybe id mmap
  where
    context = "createAllOptionalConnections"

extractModule :: Modelable a => DecodeState -> a -> Ref Module
              -> Maybe (UID', ElemOut Module ModPort)
extractModule = extractElem getDSModuleInfo getDSModulePortInfo

extractLink :: Modelable a => DecodeState -> a -> Ref Link
              -> Maybe (UID', ElemOut Link LinkPort)
extractLink = extractElem getDSLinkInfo getDSLinkPortInfo

-- | Extracts an element from the model and decodestate of the
--   system. We'll see how well it all works.
--
--   TODO :: Move all the traversal functions into the `where` block or
--           something, this is cluttered and problematic.
extractElem :: forall a b c. (Modelable c)
            => (DecodeState -> Map (Ref a) (ElemInfo a b))
            -> (DecodeState -> Map (Ref b) (PortInfo b))
            -> DecodeState
            -> c
            -> Ref a
            -> Maybe (UID', ElemOut a b)
extractElem retfun pretfun ds model elem = do
  let eim = retfun ds
  ei <- maybeThrow' ("No elemInfo found for `" ++ show elem ++ "`") $
    Map.lookup elem eim
  let eoident = ei ^. eIdent
      eouid = ei ^. eUID
  eoclass' <- extract ds model (ei ^. eClass)
  eoclass <- case eoclass' of
    Value (String s) -> return s
    _ -> fail $ "No class of correct type found in `"
      ++ show eoclass' ++ "`"
  eotype' <- extract ds model (ei ^. eType)
  eotype <- case eotype' of
    Value (Record r) -> return r
    _ -> fail $ "No type of correct type found in `"
      ++ show eotype' ++ "`"
  eoused' <- extract ds model (ei ^. eUsed)
  eoused <- case eoused' of
    Value (Bool b) -> return b
    _ -> fail $ "No used of correct types found in `"++ show eoused' ++"`"
  eoconstrained' <- extract ds model (ei ^. eConstrained)
  eoconstrained <- case eoconstrained' of
    Value (Bool b) -> return b
    _ -> fail $ "No contrained of correct types found in `"
      ++ show eoconstrained' ++"`"
  eoports <- mapM (extractPort' pretfun ds model) (ei ^. ePorts)
  eoconstraints <- flip Map.traverseWithKey (ei ^. eConstraints)
    (\ cn v -> do
      econs' <- extract ds model v
      case econs' of
        Value (Bool b) -> return b
        _ -> fail $ "Constraint `" ++ cn ++ "` in elem `" ++ show elem ++ "`"
          ++ "has value of wrong type."
    )
  eoresources <- flip Map.traverseWithKey (ei ^. eResources)
    (\ rn ResourceInfo{..} ->   do
      eused' <- extract ds model riUsed
      eused <- case eused' of
        Value (Bool b) -> return b
        _ -> fail $ "Resource `" ++ show rn ++ "` in elem `" ++ show elem ++ "`"
          ++ "has isUsed value of wrong type `" ++ show eused' ++ "`"
      euid' <- extract ds model riUid
      euid <- case euid' of
        Value (UID u) -> return u
        _ -> fail $ "Resource `" ++ show rn ++ "` in elem `" ++ show elem ++ "`"
          ++ "has uid value of wrong type `" ++ show euid' ++ "`"
      euser <- case eused of
        False -> return Nothing
        True -> Just <$> do
          euser' <- extract ds model riUser
          case euser' of
            Value (UID u) -> return u
            _ -> fail $ "Resource `" ++ show rn ++ "` in elem `"
              ++ show elem ++ "has user value of wrong type `"
              ++ show euser' ++ "`"
      return ResourceOut{roUsed=eused,roUid=euid,roUser=euser}
    )
  eoresourcecons <- flip Map.traverseWithKey (ei ^. eResourceCons)
    (\ rcn (bv,mrti) -> {- trace (show rcn) $ -} do
      ebv' <- {- (\s -> trace (show bv ++ " | " ++ show s) s) $ -}
          extract ds model bv
      ebv <- {- trace "foo" $ -} case ebv' of
        Value (Bool b) -> return b
        _ -> fail $ "ResourceConstraint `" ++ rcn ++ "` in elem `"
          ++ show elem ++ "` has expression value of wrong type `"
          ++ show ebv' ++ "`"
      emt <- case ebv of
        False -> {- trace "boo" $ -} return Nothing
        True -> {- trace "bar" $ -} Just <$> flip Map.traverseWithKey mrti
          (\ tn ResourceTagInfo{..} -> {- trace ("t: " ++ show tn) $ -} do
            eused' <- extract ds model rtiUsed
            eused <- case eused' of
              Value (Bool u) -> return u
              _ -> fail $ "ResourceConstraint `" ++ show rcn ++ "` in elem `"
                ++ show elem ++ "` has tag `" ++ tn
                ++ "` with value of wrong " ++ "type `" ++ show eused' ++ "`"
            euid' <- extract ds model rtiUid
            euid <- case euid' of
              Value (UID u) -> return u
              _ -> fail $ "ResourceCons `" ++ show rcn ++ "` in elem `"
                ++ show elem ++ "` has tag `" ++ tn ++ "` with uid of "
                ++ "wrong type `" ++ show euid' ++ "`"
            eusing <- case eused of
              False -> return Nothing
              True -> Just <$> do
                eusing' <- extract ds model rtiUsing
                case eusing' of
                  Value (UID u) -> return u
                  _ -> fail $ "ResourceCons `" ++ show rcn ++ "` in elem"
                    ++ show elem ++ "` has tag `" ++ tn ++ "` with using of"
                    ++ " wrong type `" ++ show eusing' ++ "`"
            return ResourceTagOut{rtoUsed=eused,rtoUid=euid,rtoUsing=eusing}
          )
      return (ebv, emt)
    )
  return (eouid, ElemOut{
      eoEIdent= eoident
    , eoEClass = eoclass
    , eoEType=eotype
    , eoEUID=eouid
    , eoEPorts=eoports
    , eoEUsed = eoused
    , eoEConstrained = eoconstrained
    , eoEConstraints = eoconstraints
    , eoEResources = eoresources
    , eoEResourceCons = eoresourcecons
    })
  where
    fail = error


