
module EDG.Graphviz where

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
import EDG.AssembleGraph

import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.Complete as GV
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Types.Monadic hiding (edge,(-->),(<->))
import qualified Data.GraphViz.Types.Monadic as GV
import Data.GraphViz.Commands
import Data.GraphViz.Types.Generalised (DotGraph)
import qualified Data.GraphViz.Types.Generalised as GV

-- -- | The output type of a port, what we can extract from the finished
-- --   sat solver output.
-- data PortOut a = PortOut {
--     poPName :: String
--   , poPClass :: String
--   , poPType :: Record
--   , poPConnected :: Bool
--   , poPUID :: UID'
--   , poPConnectedTo :: Maybe (UID')
--   , poPUsed :: Bool
--   , poPConstrained :: Bool
--   , poPConstraints :: Map String Bool
--   }
-- -- | The output type of an element, what we end up extracting from the
-- --   finished SATSolver output.
-- data ElemOut a b = ElemOut {
--   -- | The Identifier
--     eoEIdent :: String
--   -- | The class of element
--   , eoEClass :: String
--   -- | The type of the element
--   , eoEType :: Record
--   -- | The UID of the element
--   , eoEUID :: UID'
--   -- | Each port in the element
--   , eoEPorts :: Map PortName (UID', PortOut b)
--   -- | Was used in design?
--   , eoEUsed :: Bool
--   -- | All constraints satisfied?
--   , eoEConstrained :: Bool
--   -- | List of all standard constraints and their values for debugging.
--   , eoEConstraints :: Map String Bool
--   -- | Map of all resources to a possible resourceConstraint that
--   --   uses them.
--   , eoEResources :: Map (Resource a) ResourceOut
--   -- | The map od all resource constraints, and if fulfilled the resources
--   --   and tags that it uses.
--   , eoEResourceCons :: Map ResConName (Bool,
--     Maybe (Map ResourceTag ResourceTagOut))
--   }

-- data DecodeElem n = Elem {
--     deName :: String
--   , deIdent :: Ident
--   , deSignature :: String
--   , dePorts :: Map PortName (Maybe (Ident,PortName))
--   , deResourceConstraints :: Map ResConName
--       (Maybe (Map ResourceTag ResourceName))
--   } deriving (Eq, Show, Read)
--
-- data DecodeGraph = DecodeGraph {
--     dgLinks :: Map (Ref Link) (DecodeElem Link)
--   , dgModules :: Map (Ref Module) (DecodeElem Module)
--   } deriving (Eq, Show, Read)
--
-- data DecodeBlock = DecodeBlock{
--     dbModules :: Map (Ref Module) (ElemOut Module ModPort)
--   , dbLinks :: Map (Ref Link) (ElemOut Link LinkPort)
--   , dbGraph :: DecodeGraph
--   } deriving (Eq, Show, Read)

edge :: String -> String -> Attributes -> Dot String
edge = GV.edge

(-->) :: String -> String -> Dot String
(-->) = (GV.-->)

(<->) :: String -> String -> Dot String
(<->) = (GV.<->)

genGraph :: DecodeBlock -> DotGraph String
genGraph db@DecodeBlock{dbGraph=dg@DecodeGraph{..},..} =
  graph' $ do

    graphAttrs [
        GV.Overlap GV.ScaleXYOverlaps
      , GV.Splines GV.SplineEdges
      , GV.K 1
      ]

    cluster (Num $ GV.Int 0) $ do
      graphAttrs [style filled, color LightGray, GV.K 0.1]
      nodeAttrs [style filled, color White]
      edgeAttrs [GV.Weight (GV.Int 12)]
      "a0" --> "a1"
      "a1" --> "a2"
      "a2" --> "a3"
      graphAttrs [textLabel "process #1"]

    cluster (Num $ GV.Int 1) $ do
      nodeAttrs [style filled]
      "b0" --> "b1"
      "b1" --> "b2"
      "b2" --> "b3"
      graphAttrs [textLabel "process #2", color Blue]

    "start" --> "a0"
    "start" --> "b0"
    "a1" --> "b3"
    "b2" --> "a3"
    "a3" --> "end"
    "b3" --> "end"

    node "start" [shape MDiamond]
    node "end" [shape MSquare]

  where

    toPortName :: Ident -> PortName -> String
    toPortName i pn = i ++ ":" ++ pn

    mkModule :: Ref Module -> DecodeElem Module -> DotM String String
    mkModule = undefined

    mkLink :: Ref Link -> DecodeElem Link -> DotM String String
    mkLink = undefined



writeGraph :: DotGraph String -> FilePath -> IO FilePath
writeGraph dg fp = runGraphvizCommand Fdp dg Png fp
