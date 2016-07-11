
-- | Library Elements are the purely declarative version of elements, as found
--   within the componet library. They focus on capturing the properties of
--   elements rather than the dynamics of how they compose or interact.
module EDG.Library.Element where

-- Important Data Structures
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString)

-- ########### Component Datatypes ############

-- | Components are software and hardware objects within an embedded system
--   design.
data Component = Component {
    componentName :: ComponentName -- | UID for component type
  , componentType :: ComponentType -- | High level filtering type for component
  , componentResources :: Map ResourceName Resource -- | Internal single-use resources
  , componentProperties :: Map PropertyName Property -- | Internal properties that are unified across all ports
  , componentPorts :: Map PortName (Port,PortBindings,PortConstraints,ResourceUse) -- | Ports along with the bindings that
  , componentConstraints :: Set Constraints -- | Constraints on component, for completeness and validity
  } deriving (Show, Read, Eq, Ord)

-- | Names for components
newtype ComponentName = ComponentName String
  deriving (Show, Read, Eq, Ord, IsString)

-- | The broad type of the component, used to help filter lists as we search
--   for components of different sorts.
data ComponentType = ComponentType {
  } deriving (Show, Read, Eq, Ord)

-- | Bindings that couple the propertyname within the port to the propertyName
--   within the component.
data PortBindings = PortBindings {
    propertybindingsPort :: PropertyName
  , propertybindingsComponent :: PropertyName
  } deriving (Show, Read, Eq, Ord)

-- | Constraints that a port within a component that are specific to that
--   particular component, rather than the port itself.
data PortConstraints = PortConstraints {
  } deriving (Show, Read, Eq, Ord)

-- | Map that captures which resources the particular port uses.
type ResourceUse = Map String ResourceName

data Constraints = Constraints {
  } deriving (Show, Read, Eq, Ord)

-- TODO: Insert function stubs as needed
-- TODO: Setup the lenses as needed

-- ########### Port Datatypes ############

-- | Ports are part of component which represent abstract resource flows,
--   either inward or outward.
data Port = Port {
    portName :: PortName
  , portType :: PortType
  , portProperties :: Map PropertyName Property
  } deriving (Show, Read, Eq, Ord)

-- | Names for port
newtype PortName = PortName String
  deriving (Show, Read, Eq, Ord, IsString)

data PortType = PortType {
  } deriving (Show, Read, Eq, Ord)

{- Possible port types:
 - Power I
   - Voltage
   - Current Supplied
   - Shared Ground
 - Power O
   - VOltage
 - SW
 - Digital I/O/IO
 - Analog I/O
 - I2C
 - SPI
 - UART
 - Programming I/O
 -}

-- ########### Link Datatypes ############

-- | Links are connections between ports which show that they happen to
--   connect in a way that has some property.
data Link = Link {
  } deriving (Show, Read, Eq, Ord)

{- Possible connection types:
 - Power
 - SW
 - Digital
 - Analog
 - I2C
 - SPI
 - UART
 - Programming
 -}

-- ########### Resource Datatypes ############

-- | Resources are component-internal objects that must be allocated between
--   ports.
data Resource = Resource {
  } deriving (Show, Read, Eq, Ord)

-- | Names for properties
newtype ResourceName = ResourceName String
  deriving (Show, Read, Eq, Ord, IsString)

-- ########### Property Datatypes ############

-- | Properties are lattice-like values that are propagated across the
--   design for verification purposes.
data Property = Property {
  } deriving (Show, Read, Eq, Ord)

-- | Names for properties
newtype PropertyName = PropertyName String
  deriving (Show, Read, Eq, Ord, IsString)

-- ########### Property Datatypes ############
