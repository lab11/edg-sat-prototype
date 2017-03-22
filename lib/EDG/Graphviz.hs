
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

import Control.Monad

import Debug.Trace

import Data.Void
import Data.List (intersperse)
import Data.Maybe (fromJust)

import EDG.Elements.Port
import EDG.Elements.Elem
import EDG.AssembleGraph

import Data.GraphViz.Attributes hiding (Record)
import qualified Data.GraphViz.Attributes.Complete as GV
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Types.Monadic hiding (edge,(-->),(<->))
import qualified Data.GraphViz.Types.Monadic as GV
import Data.GraphViz.Commands
import Data.GraphViz.Types.Generalised (DotGraph)
import qualified Data.GraphViz.Types.Generalised as GV

import qualified Data.Text.Lazy as T

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

type BlockName = String

genGraph :: DecodeBlock -> DotGraph String
genGraph db@DecodeBlock{dbGraph=dg@DecodeGraph{..},..} =
  graph' $ do
    graphAttrs [
        GV.Overlap GV.ScaleXYOverlaps
      , GV.Splines GV.SplineEdges
      ]
    mapM_ mkModule (Map.keys dbModules)
    mapM_ mkLink (Map.keys dbLinks)


  --  cluster (Num $ GV.Int 0) $ do
  --    graphAttrs [style filled, color LightGray, GV.K 0.1]
  --    nodeAttrs [style filled, color White]
  --    edgeAttrs [GV.Weight (GV.Int 12)]
  --    "a0" --> "a1"
  --    "a1" --> "a2"
  --    "a2" --> "a3"
  --    graphAttrs [textLabel "process #1"]

  --  cluster (Num $ GV.Int 1) $ do
  --    nodeAttrs [style filled]
  --    "b0" --> "b1"
  --    "b1" --> "b2"
  --    "b2" --> "b3"
  --    graphAttrs [textLabel "process #2", color Blue]

  --  "start" --> "a0"
  --  "start" --> "b0"
  --  "a1" --> "b3"
  --  "b2" --> "a3"
  --  "a3" --> "end"
  --  "b3" --> "end"

  --  node "start" [shape MDiamond]
  --  node "end" [shape MSquare]

  where

    theme = defaultTheme

    toPortName :: BlockName -> PortName -> String
    toPortName bn pn = bn ++ ":" ++ pn

    mkModule :: Ref Module -> DotM String String
    mkModule rm = do
      cluster (Str $ T.pack name) $ do
        -- Set the attributes of modules
        graphAttrs [style filled, color LightBlue]
        nodeAttrs [style filled, color White]
        graphAttrs [textLabel $ T.pack name]

        -- Make the center element
        center <- mkBlockData decodeElem elemOut

        forM_ (Map.assocs $ eoEPorts elemOut) $ \ (pn,(_,po)) -> do
          -- Make the port block
          v <- mkPort name pn po
          -- attach it to the center element
          center <-> v
          return v

      return name
      where
        -- NOTE :: This is fragile, if this isn't the unpacked reference
        --         then everything breaks. It's a damn shame.
        name = unpack rm

        decodeElem = case Map.lookup rm dgModules of
          Nothing -> error $ "No decodeElem for module `" ++ show rm ++ "`"
          Just de -> de

        elemOut = case Map.lookup rm dbModules of
          Nothing -> error $ "No elemOut for module `" ++ show rm ++ "`"
          Just eo -> eo


    mkLink :: Ref Link -> DotM String String
    mkLink rl = do
      cluster (Str $ T.pack name) $ do
        graphAttrs [style filled, color LightGray]
        nodeAttrs [style filled, color White]
        -- Set the attributes of modules
        graphAttrs [textLabel $ T.pack name]

        -- Make the center element
        center <- mkBlockData decodeElem elemOut

        -- for each port
        forM_ (Map.assocs $ eoEPorts elemOut) $ \ (pn,(_,po)) -> do
          -- Make the actual port
          v <- mkPort name pn po
          -- Connect it to the center
          center <-> v

      -- Go through each port and attach them to their counterparts
      forM_ (Map.assocs $ eoEPorts elemOut)  $ \ (pn,(_,po)) ->
        case Map.lookup pn $ dePorts decodeElem of
          -- There should be something here
          Nothing -> error $ "Port `" ++ pn ++ "` not found in decodeGraph"
            ++ " for link `" ++ show rl ++ "`"
          -- but there might not be a connection
          Just (Nothing) -> return ()
          -- If there is we should connect things up properly.
          Just (Just (id,pn')) -> (toPortName name pn) <-> (toPortName id pn')
      return name
      where
        name = unpack rl

        decodeElem = case Map.lookup rl dgLinks of
          Nothing -> error $ "No decodeElem for link `" ++ show rl ++ "`"
          Just de -> de

        elemOut = case Map.lookup rl dbLinks of
          Nothing -> error $ "No elemOut for link `" ++ show rl ++ "`"
          Just eo -> eo

    mkBlockData :: DecodeElem a -> ElemOut a b -> DotM String String
    mkBlockData de@Elem{..} eo@ElemOut{..} = do
      node name [
          GV.Shape GV.PlainText
        , GV.Label label
        , color Lavender
        ]
      return name
      where
        name = deName ++ ":centerBlockGraphViz"
        label = GV.HtmlLabel . H.Table $ H.HTable{
            H.tableFontAttrs = Nothing
          , H.tableAttrs = []
          , H.tableRows = [
              H.Cells [headerCell deName]
            , H.Cells [dataCell theme "Signature" deSignature]
            , H.Cells [dataCell theme "Ident" deIdent]
            , H.Cells [uidCell theme "UID" eoEUID]
            , H.Cells [typeCell theme "Type" eoEType]
            ]
          }

    mkPort :: BlockName -> PortName -> PortOut n -> DotM String String
    mkPort bn pn po@PortOut{..} = do
      node name [
          GV.Shape GV.PlainText
        , GV.Label label
        ]
      return name
      where
        name = toPortName bn pn
        label = GV.HtmlLabel . H.Table $ H.HTable{
            H.tableFontAttrs = Nothing
          , H.tableAttrs = []
          , H.tableRows = [
              H.Cells [headerCell poPName]
            , H.Cells [dataCell theme "PortName" pn]
            , H.Cells [dataCell theme "Kind" poPClass]
            , H.Cells [uidCell theme "UID" poPUID]
            , H.Cells [boolCell theme "Connected" poPConnected]
            , H.Cells [typeCell theme "Type" poPType]
            ]
          }



writeGraph :: DotGraph String -> FilePath -> IO FilePath
writeGraph dg fp = runGraphvizCommand Fdp dg Png fp

-- * Shit to do with formatting *

data Theme = Theme {
    constructorFont :: [H.Attribute]
  , boolFont        :: [H.Attribute]
  , stringFont      :: [H.Attribute]
  , intFont         :: [H.Attribute]
  , floatFont       :: [H.Attribute]
  , uidFont         :: [H.Attribute]
  , constraintFont  :: [H.Attribute]
  , resconsFont     :: [H.Attribute]
  , resourceFont    :: [H.Attribute]
  , fieldFont       :: [H.Attribute]
  , opFont          :: [H.Attribute]
  , dataFont        :: [H.Attribute]
  }

defaultTheme :: Theme
defaultTheme = Theme{
    constructorFont = [H.Color $ GV.X11Color NavyBlue      ]
  , boolFont        = [H.Color $ GV.X11Color DarkOliveGreen]
  , stringFont      = [H.Color $ GV.X11Color SlateGray     ]
  , intFont         = [H.Color $ GV.X11Color Maroon        ]
  , floatFont       = [H.Color $ GV.X11Color Maroon        ]
  , uidFont         = [H.Color $ GV.X11Color DarkOrange2   ]
  , constraintFont  = [H.Color $ GV.X11Color DarkViolet    ]
  , resconsFont     = [H.Color $ GV.X11Color DarkViolet    ]
  , resourceFont    = [H.Color $ GV.X11Color DarkTurquoise ]
  , fieldFont       = [H.Color $ GV.X11Color DarkGreen]
  , opFont          = [H.Color $ GV.X11Color Cyan4]
  , dataFont        = []
  }

pattern Bold a = H.Format H.Bold a
pattern Italics a = H.Format H.Bold a

headerCell :: String -> H.Cell
headerCell s = H.LabelCell [] $ H.Text [Bold [Italics [H.Str $ T.pack s]]]

dataCell :: Theme -> String -> String -> H.Cell
dataCell Theme{..} label dat = H.LabelCell [H.Align H.HLeft]
    $  H.Text [Bold [H.Str . T.pack $ label ++ ": "]
        , H.Font dataFont [H.Str . T.pack $ dat]]

mkDataCell :: (Theme -> a -> [H.TextItem]) -> Theme -> String -> a -> H.Cell
mkDataCell toText theme label dat = H.LabelCell [H.Align H.HLeft]
    $ H.Text (Bold [H.Str . T.pack $ label ++ ": "] : toText theme dat)


boolText :: Theme -> Bool -> [H.TextItem]
boolText Theme{..} True  = [H.Font boolFont [H.Str "True" ]]
boolText Theme{..} False = [H.Font boolFont [H.Str "False"]]

boolCell :: Theme -> String -> Bool -> H.Cell
boolCell = mkDataCell boolText

stringText :: Theme -> String -> [H.TextItem]
stringText Theme{..} s = [H.Font stringFont [H.Str . T.pack . show $ s]]

stringCell :: Theme -> String -> String -> H.Cell
stringCell = mkDataCell stringText

intText :: Theme -> Integer -> [H.TextItem]
intText Theme{..} i = [H.Font intFont [H.Str . T.pack . show $ i]]

intCell :: Theme -> String -> Integer -> H.Cell
intCell = mkDataCell intText

floatText :: Theme -> Float -> [H.TextItem]
floatText Theme{..} f = [H.Font floatFont [H.Str . T.pack . show $ f]]

floatCell :: Theme -> String -> Float -> H.Cell
floatCell = mkDataCell floatText

uidText :: Theme -> UID' -> [H.TextItem]
uidText Theme{..} u = [H.Font uidFont [H.Str . T.pack . show . unpack $ u]]

uidCell :: Theme -> String -> UID' -> H.Cell
uidCell = mkDataCell uidText

spaceText :: [H.TextItem]
spaceText = [H.Str . T.pack $ " "]

lConcat :: [[s]] -> [s]
lConcat = concat

valueText :: Theme -> Value -> [H.TextItem]
valueText theme@Theme{..} (getValue -> v)
  | Int i <- v = appendCons "Int" intText i
  | Bool b <- v = appendCons "Bool" boolText b
  | Float f <- v = appendCons "Float" floatText f
  | String s <- v = appendCons "String" stringText s
  | UID u <- v = appendCons "UID" uidText u
  | Record r <- v = appendCons "Record" recordText r
  | KVTop a <- v = absurd a
  | KVBot a <- v = absurd a
  where
    appendCons :: String -> (Theme -> a -> [H.TextItem]) -> a -> [H.TextItem]
    appendCons cons toText dat = lConcat [
        constructorText theme cons
      , spaceText
      , toText theme dat
      ]

valueCell :: Theme -> String -> Value -> H.Cell
valueCell = mkDataCell valueText

recordText :: Theme -> Record -> [H.TextItem]
recordText theme@Theme{..}
  = {-- (prefix ++) . (++ suffix) .--} indent . concatMap field . Map.assocs . rMap
  where
    prefix :: [H.TextItem]
    prefix = opText theme "{"

    suffix :: [H.TextItem]
    suffix = newline ++ opText theme "}"

    field :: (String,Value) -> [H.TextItem]
    field (s,v) = lConcat [
        newline
      , fieldText theme s
      , spaceText
      , opText theme "<:="
      , spaceText
      , valueText theme v
      ]

recordCell :: Theme -> String -> Record -> H.Cell
recordCell = mkDataCell recordText

typeText :: Theme -> Record -> [H.TextItem]
typeText t r = valueText t (Value (Record r)) ++ newline

typeCell :: Theme -> String -> Record -> H.Cell
typeCell = mkDataCell typeText

constructorText :: Theme -> String -> [H.TextItem]
constructorText Theme{..} s = [H.Font constructorFont [H.Str . T.pack $ s]]

constraintText :: Theme -> String -> [H.TextItem]
constraintText Theme{..} s = [H.Font constraintFont [H.Str . T.pack $ s]]

resconsText :: Theme -> String -> [H.TextItem]
resconsText Theme{..} s = [H.Font resconsFont [H.Str . T.pack $ s]]

fieldText :: Theme -> String -> [H.TextItem]
fieldText Theme{..} s = [Italics [H.Font fieldFont [H.Str . T.pack $ s]]]

opText :: Theme -> String -> [H.TextItem]
opText Theme{..} s = [Bold [H.Font opFont [H.Str . T.pack $ s]]]

indentItem :: H.TextItem -> [H.TextItem]
indentItem (H.Str s)
  = [H.Str "    ", H.Str s]
indentItem f@(H.Font a l)
  = [H.Font a $ indent l]
indentItem f@(H.Format a l)
  = [H.Format a $ indent l]
indentItem n@(H.Newline _) = [n]

indentNL :: [H.TextItem] -> [H.TextItem]
indentNL (n@(H.Newline _):ts) = n : indent ts
indentNL (t:ts) = t : indentNL ts
indentNL [] = []

indent :: [H.TextItem] -> [H.TextItem]
indent (t@(H.Newline _):ts) = t : indent ts
indent (t:ts) = indentItem t ++ indentNL ts
indent [] = []

newline :: [H.TextItem]
newline = [H.Newline [H.Align H.HLeft]]
