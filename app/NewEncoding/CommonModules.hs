module NewEncoding.CommonModules where

import Control.Monad

import EDG
import NewEncoding.CommonPorts

-- A button module
button :: Module ()
button = do
  setIdent "button"
  setSignature "button"
  setType []
  vin <- addPort "vin" $ do
    electricalPort
    setType [
        "current" <:= FloatV 0.001  -- for when button closes and resistor shorts to ground
      , "dir" <:= StringV "sink"
      , "control" <:= noControl
      ]
    return ()
  out <- addPort "out" $ do
    digitalPort
    setType [
        "current" <:= FloatV 0  -- this resistor-switch topology doesn't allow current draw
      , "dir" <:= StringV "source"
      , "control" <:= onOffControl
      ]
    return ()
  constrain $ port out (typeVal "control.dir") :== Lit (StringV "consumer")

  seed <- addPort "seed" $ do
    seedPort
    setType [
        "control" <:= Record [
            "api" <:= StringV "button"
          , "name" <:= StringC unknown
          , "dir" <:= StringV "producer"
          , "data" <:= Record [
              "bandwidth" <:= FloatC unknown
            ]
          ]
      ]
    return ()

  constrain $ port vin connected
  constrain $ port out connected
  constrain $ port out (typeVal "control.name") :== port seed (typeVal "control.name")
  constrain $ port out (typeVal "control.data") :== port seed (typeVal "control.data")

  constrain $ port vin (typeVal "voltage") :== port out (typeVal "voltage")
  -- TODO digital signal threhsolds

-- A LED Module
led :: Module ()
led = do
  setIdent "led"
  setSignature "led"
  setType []
  source <- addPort "source" $ do
    digitalPort
    setType [
        "current" <:= FloatV 0.01
      , "dir" <:= StringV "sink"
      , "control" <:= onOffControl
      ]
    return ()
  constrain $ port source (typeVal "control.dir") :== Lit (StringV "consumer")

  seed <- addPort "seed" $ do
    seedPort
    setType [
        "control" <:= Record [
            "api" <:= StringV "led"
          , "name" <:= StringC unknown
          , "dir" <:= StringV "producer"
          , "data" <:= Record [
              "bandwidth" <:= FloatC unknown
            ]
          ]
      ]
    return ()

  constrain $ port source connected
  constrain $ port source (typeVal "control.name") :== port seed (typeVal "control.name")
  constrain $ port source (typeVal "control.data") :== port seed (typeVal "control.data")
  constrain $ port source (typeVal "voltage") :>= Lit (FloatV 3.0)  -- LED voltage drop

  -- TODO digital signal threhsolds

mcu :: Module ()
mcu = do
  setIdent "Microcontroller"
  setSignature "Microcontroller"
  setType [
      "MHz" <:= FloatV 8.0  -- TODO: is this useful?
    ]
  usbIn <- addPort "UsbIn" $ do
    dummyElectricalPort
    setType [
        "voltage" <:= FloatV 5.0
      , "current" <:= FloatC $ 0.25 +/- 0.25
      , "dir" <:= StringV "sink"
      ]
    return ()

  constrain $ Not (port usbIn connected)  -- dummy port only

  p5vOut <- addPort "5vOut" $ do
    electricalPort
    setType [
        "dir" <:= StringV "source"
      ]
    return ()

  constrain $ port p5vOut (typeVal "voltage") :== port usbIn (typeVal "voltage")


  p3v3Out <- addPort "3v3Out" $ do
    electricalPort
    setType [
        "voltage" <:= FloatV 3.3
      , "dir" <:= StringV "source"
      ]
    return ()

  gpios <- forM [1..4] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalPort
      setType [
          "voltage" <:= FloatV 3.3
        , "control" <:= onOffControl
        ]
      return ()

  constrain $ port usbIn (typeVal "current") :== Sum (
    (port p5vOut $ typeVal "current") :
    (port p3v3Out $ typeVal "current") :
    (map (\ gpio -> port gpio (typeVal "current")) gpios))

  endDef


















