module NewEncoding.CommonPorts where

import EDG

-- Electrical Ports

electricalPort :: (IsPort p) => p ()
electricalPort = do
  setKind "ElectricalPort"
  setIdent "ElectricalPort"
  setType [
      "voltage" <:= FloatC unknown
    , "current" <:= FloatC unknown
    , "dir" <:= StringC $ oneOf ["source", "sink", "bi"]
    , "uid" <:= UID
    ]
  return ()

digitalPort :: (IsPort p) => p ()
digitalPort = do
  electricalPort
  setKind "DigitalPort"
  setIdent "DigitalPort"
  setType [
      -- Note: re-use electricalPort's dir for signal direction,
      -- since logical signal flow matches electrical flow
      -- But for now, digital signals must be unidirectional
      "dir" <:= StringC $ oneOf ["source", "sink"]

      -- TODO: Signal threhsold levels
    ]
  return ()

-- Electrical Links
electricalLink :: Link ()
electricalLink = do
  setIdent "ElectricalLink"
  setSignature "ElectricalLink"

  source <- addPort "source" $ do
    electricalPort
    setType ["dir" <:= StringV "source"]
    return()

  sink <- addPort "sink" $ do
    electricalPort
    setType ["dir" <:= StringV "sink"]
    return()

  constrain $ port source connected
  constrain $ port sink connected
  constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port source (typeVal "current") :== port sink (typeVal "current")

  return ()

digitalLink :: Link ()
digitalLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  source <- addPort "source" $ do
    digitalPort
    setType ["dir" <:= StringV "source"]
    return()

  sink <- addPort "sink" $ do
    digitalPort
    setType ["dir" <:= StringV "sink"]
    return()

  constrain $ port source connected
  constrain $ port sink connected
  constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port source (typeVal "current") :== port sink (typeVal "current")

  return ()

-- Control Ports

controlPort :: (IsPort p) => p ()
controlPort = do
  setType [
      "apiType" <:= StringC unknown
    , "name" <:= StringC unknown
    , "dir" <:= StringC $ oneOf ["producer", "consumer"]
    , "uid" <:= UID
    ]
  return ()

genericGpio :: (IsPort p) => p ()
genericGpio = do
  controlPort
  setIdent "GenericGpioApi"
  setKind "GenericGpioApi"
  setType [
      "bandwidth" <:= FloatC unknown
    ]
  return ()

gpioControl :: (IsPort p) => p ()
gpioControl = do
  genericGpio
  setIdent "GpioApi"
  setType [
      "apiType" <:= StringV "gpio"
    ]
  return ()

gpioControlLink :: Link ()
gpioControlLink = do
  setIdent "GpioControlLink"
  setSignature "GpioControlLink"

  producer <- addPort "producer" $ do
    genericGpio
    setType ["dir" <:= StringV "producer"]
    return()

  consumer <- addPort "consumer" $ do
    genericGpio
    setType ["dir" <:= StringV "consumer"]
    return()

  constrain $ port producer connected
  constrain $ port consumer connected
  constrain $ port producer (typeVal "apiType") :== port consumer (typeVal "apiType")
  constrain $ port producer (typeVal "name") :== port consumer (typeVal "name")
  constrain $ port producer (typeVal "bandwidth") :== port consumer (typeVal "bandwidth")
  constrain $ port producer (typeVal "uid") :== port consumer (typeVal "uid")
  -- TODO: UIDs

  return ()
