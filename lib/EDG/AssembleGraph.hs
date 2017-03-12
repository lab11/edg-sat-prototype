
module EDG.AssembleGraph where

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

import EDG.EDGMonad

import Control.Monad.Ether.Implicit
import Control.Lens.Ether.Implicit

import EDG.PortTypes
import EDG.ElemTypes
import EDG.Library.Types
import EDG.Expression
import EDG.ElemTypes
import EDG.EDGDatatype
import EDG.EDGMonad hiding (trace)
import EDG.EDGInstances

import Debug.Trace

import Data.Maybe (fromJust)

import EDG.Elements.Port
import EDG.Elements.Elem

import Data.SBV (
    Boolean,(|||),(&&&),(~&),(~|),(<+>),(==>),(<=>),sat,allSat
  , SatResult(..), SMTResult(..), SMTConfig(..), CW(..), Kind(..), Modelable(..)
  )


data UIDData = UIDData {
    udModPorts  :: Map UID' (Ref ModPort )
  , udLinkPorts :: Map UID' (Ref LinkPort)
  , udModules   :: Map UID' (Ref Module  )
  , udLinks     :: Map UID' (Ref Link    )
  , udModPortParents  :: Map (Ref ModPort ) (String, Ref Module)
  , udLinkPortParents :: Map (Ref LinkPort) (String, Ref Link  )
}

deriving instance Eq   UIDData
deriving instance Show UIDData
deriving instance Read UIDData

buildUIDData :: GatherState -> UIDData
buildUIDData gs = UIDData {
    udModPorts=modPorts
  , udLinkPorts=linkPorts
  , udModules=modules
  , udLinks=links
  , udModPortParents=modPortParents
  , udLinkPortParents=linkPortParents
  }
  where

    flipMap :: Ord b => Map a b -> Map b a
    flipMap = Map.fromList . map (\ (a,b) -> (b,a)) . Map.toList

    modPorts :: Map UID' (Ref ModPort )
    modPorts = flipMap . Map.map (^. pUid) $ gs ^. modulePortInfo

    linkPorts :: Map UID' (Ref LinkPort)
    linkPorts = flipMap . Map.map (^. pUid) $ gs ^. linkPortInfo

    modules :: Map UID' (Ref Module  )
    modules = flipMap . Map.map (^. eUID) $ gs ^. moduleInfo

    links :: Map UID' (Ref Link    )
    links = flipMap . Map.map (^. eUID) $ gs ^. linkInfo

    getParents :: Map (Ref a) (ElemInfo a b) -> Map (Ref b) (String, Ref a)
    getParents = Map.fromList . concat . Map.elems
      . Map.mapWithKey (\ k s -> map (\ (pn,r) -> (r,(pn,k)))
        . Map.toList $ s ^. ePorts)

    modPortParents :: Map (Ref ModPort ) (String, Ref Module)
    modPortParents = getParents $ gs ^. moduleInfo

    linkPortParents :: Map (Ref LinkPort) (String, Ref Link  )
    linkPortParents = getParents $ gs ^. linkInfo

type Ident = String
type ResourceName = String

data DecodeElem n = Elem {
    deName :: Ref n
  , deIdent :: Ident
  , dePorts :: Map PortName (Maybe (Ident,PortName))
  , deResourceConstraints :: Map ResConName
      (Maybe (Map ResourceTag ResourceName))
  } deriving (Eq, Show, Read)

data DecodeGraph = DecodeGraph {
    dgLinks :: Map (Ref Link) (DecodeElem Link)
  , dgModules :: Map (Ref Module) (DecodeElem Module)
  } deriving (Eq, Show, Read)

data DecodeBlock = DecodeBlock{
    dbModules :: Map (Ref Module) (ElemOut Module ModPort)
  , dbLinks :: Map (Ref Link) (ElemOut Link LinkPort)
  , dbGraph :: DecodeGraph
  } deriving (Eq, Show, Read)



data IncDecState = IncDecState {
    idsBlock :: DecodeBlock
  , idsLinkQueue :: Set (Ref Link)
  , idsModuleQueue :: Set (Ref Module)
  } deriving (Eq, Show, Read)

type IDC = IncDecState

-- type DecodeM = StateT IncDecState (Except String)

makeLensesWith abbreviatedFields ''UIDData
makeLensesWith abbreviatedFields ''DecodeElem
makeLensesWith abbreviatedFields ''DecodeGraph
makeLensesWith abbreviatedFields ''DecodeBlock
makeLensesWith abbreviatedFields ''IncDecState

decodeResult :: Modelable a
             => DecodeState
             -> a
             -> Ref Module -- Seed from which we start the decode process.
             -> Either String DecodeBlock
decodeResult ds model seed = idsBlock <$> decodeStep IncDecState{
      idsBlock = DecodeBlock{
            dbModules = mempty
          , dbLinks = mempty
          , dbGraph = DecodeGraph {
                dgLinks = mempty
              , dgModules = mempty
            }
        }
    , idsLinkQueue = mempty
    , idsModuleQueue = Set.singleton seed
  }
  where
    gs = ds ^. _1

    uidData = buildUIDData $ ds ^. _1

    update :: Lens' s a -> (a -> (b,a)) -> s -> (b,s)
    update l u s = (\ (b,a) -> (b,l .~ a $ s)) $ u (s ^. l)

    -- Step through, pop something off a queue and decode it if needed.
    decodeStep :: IDC -> Either String IDC
    decodeStep idc
      -- If there's modules in the queue, decode that
      | not (Set.null $ idc ^. moduleQueue) = decodeStep =<<
        (decodeModule . (update moduleQueue Set.deleteFindMin) $ idc)
      -- Likewise for links
      | not (Set.null $ idc ^. linkQueue  ) = decodeStep =<<
        (decodeLink . (update linkQueue Set.deleteFindMin) $ idc)
      -- otherwise we're done.
      | otherwise = Right idc

    decodeModule :: (Ref Module, IDC) -> Either String IDC
    decodeModule (rm,idc)
      -- already did this one, move on
      | Map.member rm (idc ^. block . modules) = Right idc
      -- actual work this time :V
      | Nothing <- modOut = Left $ "Could not extract module `" ++ show rm
      | Just mo <- modOut = let em = makeDEM rm mo in
          Right . (block . modules %~ Map.insert rm mo)
                . (block . graph . modules %~ Map.insert rm em) $ idc
        where
          modOut = snd <$> extractModule ds model rm

    decodeLink :: (Ref Link, IDC) -> Either String IDC
    decodeLink (rl,idc)
      -- already did this one, move on
      | Map.member rl (idc ^. block . links) = Right idc
      -- actual work this time
      | Nothing <- linkOut = Left $ "Could not extract link `" ++ show rl
      | Just lo <- linkOut = let el = makeDEL rl lo in
          Right . (block . links %~ Map.insert rl lo)
                . (block . graph . links %~ Map.insert rl el) $ idc
        where
          linkOut = snd <$> extractLink ds model rl

    makeDEL :: Ref Link ->  ElemOut Link LinkPort -> DecodeElem Link
    makeDEL = makeDE linkPorts linkPortParents

    makeDEM :: Ref Module ->  ElemOut Module ModPort -> DecodeElem Module
    makeDEM = makeDE modPorts modPortParents

    makeDE :: ()
           => Lens' UIDData (Map (UID') (Ref b))
           -> Lens' UIDData (Map (Ref b) (String,Ref a))
           -> Ref a ->  ElemOut a b -> DecodeElem a
    makeDE pulens parlens r ElemOut{..} = Elem{
        -- deName :: Ref n
        deName = r
        -- deIdent :: Ident
      , deIdent = eoEIdent
        -- dePorts :: Map PortName (Maybe (Ident,PortName))
      , dePorts = ports
        -- deResourceConstraints :: Map ResConName
        --   (Maybe (Map ResourceTag ResourceName))
      , deResourceConstraints = recCons
      }
      where
        ports = flip Map.map eoEPorts (\ (_, po@PortOut{..}) -> do
            connUID <- poPConnectedTo
            connRef <- Map.lookup connUID (uidData ^. pulens)
            (portname,parRef) <- Map.lookup connRef (uidData ^. parlens)
            return (unpack parRef,portname)
          )

        recCons = Map.map (fmap $ Map.map lookupResName). Map.map snd
          $ eoEResourceCons

        lookupResName :: ResourceTagOut -> ResourceName
        lookupResName (fromJust . rtoUsing -> uid)
          = unpack . fst . Map.findMin . Map.filter ((==) uid . roUid)
            $ eoEResources
