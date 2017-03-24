module NewEncoding.CommonPorts where

import EDG

-- Control Schemes
baseControl :: () => AmbigVal
baseControl = Record [
    "scheme" <:= StringC unknown
  , "dir" <:= StringC $ oneOf ["none", "producer", "consumer"]
  , "data" <:= Record unknown
  ]

noControl :: () => AmbigVal
noControl = Record [
    "scheme" <:= StringV "None"
  , "dir" <:= StringV "none"
  , "data" <:= Record []
  ]

onOffControl :: () => AmbigVal
onOffControl = Record [
    "scheme" <:= StringV "OnOff"
  , "dir" <:= StringC $ oneOf ["producer", "consumer"]
  , "data" <:= Record [
        "bandwidth" <:= FloatC unknown
      ]
  ]

pwmControl :: () => AmbigVal
pwmControl = Record [
    "scheme" <:= StringV "pwm"
  , "dir" <:= StringC $ oneOf ["producer", "consumer"]
  , "data" <:= Record [
        "period" <:= FloatC unknown
      , "bandwidth" <:= FloatC unknown
      ]
  ]

-- Electrical Ports

electricalPort :: (IsPort p) => p ()
electricalPort = do
  setKind "ElectricalPort"
  setIdent "ElectricalPort"
  setType [
      "voltage" <:= FloatC unknown
    , "current" <:= FloatC unknown
    , "dir" <:= StringC $ oneOf ["source", "sink", "bi"]  -- current flow direction
    , "controlScheme" <:= Record unknown  -- optional control scheme
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
