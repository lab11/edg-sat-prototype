module NewEncoding.CommonPorts where

import EDG

-- Control Schemes
baseControl :: () => AmbigVal
baseControl = Record [
    "api" <:= StringC unknown
  , "name" <:= StringC unknown
  , "dir" <:= StringC $ oneOf ["none", "producer", "consumer"]
  , "data" <:= Record unknown
  ]

noControl :: () => AmbigVal
noControl = Record [
    "api" <:= StringV "None"
  , "name" <:= StringV "none"
  , "dir" <:= StringV "none"
  , "data" <:= Record []
  ]

onOffControl :: () => AmbigVal
onOffControl = Record [
    "api" <:= StringV "OnOff"
  , "name" <:= StringC unknown
  , "dir" <:= StringC $ oneOf ["producer", "consumer"]
  , "data" <:= Record [
        "bandwidth" <:= FloatC unknown
      ]
  ]

pwmControl :: () => AmbigVal
pwmControl = Record [
    "api" <:= StringV "pwm"
  , "name" <:= StringC unknown
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
    , "control" <:= Record unknown  -- optional control scheme
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

  constrain $ port source (typeVal "control.api") :== port sink (typeVal "control.api")
  constrain $ port source (typeVal "control.name") :== port sink (typeVal "control.name")
  constrain $ (
      ((port source (typeVal "control.dir") :== Lit (StringV "producer"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "consumer"))
      ) :|| (
        (port source (typeVal "control.dir") :== Lit (StringV "consumer"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "producer"))
      ) :|| (
        (port source (typeVal "control.dir") :== Lit (StringV "none"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "none"))
      ))

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

  constrain $ port source (typeVal "control.api") :== port sink (typeVal "control.api")
  constrain $ port source (typeVal "control.name") :== port sink (typeVal "control.name")
  constrain $ (
      ((port source (typeVal "control.dir") :== Lit (StringV "producer"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "consumer"))
      ) :|| (
        (port source (typeVal "control.dir") :== Lit (StringV "consumer"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "producer"))
      ) :|| (
        (port source (typeVal "control.dir") :== Lit (StringV "none"))
        :&& (port sink (typeVal "control.dir") :== Lit (StringV "none"))
      ))

  return ()

seedPort :: (IsPort p) => p ()
seedPort = do
  setKind "SeedPort"
  setIdent "SeedPort"
  setType [
      "control" <:= Record unknown  -- optional control scheme
      -- TODO: add device-specific records
    ]
  return ()

-- Seed Links
seedLink :: Link ()
seedLink = do
  setIdent "SeedLink"
  setSignature "SeedLink"

  producer <- addPort "consumer" $ do
    seedPort
    return()

  consumer <- addPort "producer" $ do
    seedPort
    return()

  constrain $ port producer connected
  constrain $ port consumer connected

  constrain $ port producer (typeVal "control.api") :== port consumer (typeVal "control.api")
  constrain $ port producer (typeVal "control.name") :== port consumer (typeVal "control.name")
  -- TODO: breaks for unknown reasons!
  --constrain $ port producer (typeVal "control.data") :== port consumer (typeVal "control.data")
  constrain $ port producer (typeVal "control.dir") :== Lit (StringV "producer")
  constrain $ port consumer (typeVal "control.dir") :== Lit (StringV "consumer")
  return ()
