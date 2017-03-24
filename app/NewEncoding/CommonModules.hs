module NewEncoding.CommonModules where

import Control.Monad

import EDG
import NewEncoding.CommonPorts

-- A button module
buttonControl :: (IsPort p) => p ()
buttonControl = do
  genericGpio
  setIdent "ButtonApi"
  setType [
      "apiType" <:= StringV "button"
    ]
  return ()

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
      ]
    return ()
  out <- addPort "out" $ do
    digitalPort
    setType [
        "current" <:= FloatV 0  -- this resistor-switch topology doesn't allow current draw
      , "dir" <:= StringV "source"
      ]
    return ()
  swGpio <- addPort "gpioApi" $ do
    gpioControl
    setType [
        "dir" <:= StringV "consumer"
      ]
    return ()
  swButton <- addPort "buttonApi" $ do
    buttonControl
    setType [
        "dir" <:= StringV "producer"
      ]
    return ()
  --constrain $ port vin connected
  --constrain $ port out connected
  --constrain $ port swButton connected :== port swGpio connected
  constrain $ port swButton (typeVal "name") :== port swGpio (typeVal "name")
  constrain $ port swButton (typeVal "bandwidth") :== port swGpio (typeVal "bandwidth")
  constrain $ port swButton (typeVal "uid") :== port swGpio (typeVal "uid")

  constrain $ port vin (typeVal "voltage") :== port out (typeVal "voltage")
  -- TODO digital signal threhsolds

-- A LED Module

ledControl :: (IsPort p) => p ()
ledControl = do
  genericGpio
  setIdent "LedApi"
  setType [
      "apiType" <:= StringV "led"
    ]
  return ()

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
      ]
    return ()
  swGpio <- addPort "gpioApi" $ do
    gpioControl
    setType [
        "dir" <:= StringV "consumer"
      ]
    return ()
  swLed <- addPort "ledApi" $ do
    ledControl
    setType [
        "dir" <:= StringV "producer"
      ]
    return ()
  constrain $ port source connected
  constrain $ port swLed connected :== port swGpio connected
  constrain $ port source (typeVal "uid") :== port swGpio (typeVal "uid")  -- must be together
  constrain $ port source (typeVal "voltage") :>= Lit (FloatV 3.0)  -- LED voltage drop
  constrain $ port swLed (typeVal "bandwidth") :== port swGpio (typeVal "bandwidth")
  -- TODO digital signal threhsolds

mcu :: Module ()
mcu = do
  setIdent "Microcontroller"
  setSignature "Microcontroller"
  setType [
      "MHz" <:= FloatV 8.0  -- TODO: is this useful?
    ]
  usbIn <- addPort "UsbIn" $ do
    electricalPort
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

  gpios <- forM ["gpio1","gpio2"] $ \ name ->
    addPort name $ do
      digitalPort
      setType [
          "voltage" <:= FloatV 3.3
        ]
      return ()

  gpioSw <- addPort "gpio1Sw" $ do
    gpioControl
    setType [
        "bandwidth" <:= FloatC $ 500 +/- 500
      , "dir" <:= StringV "producer"
      ]
    return ()

  constrain $ port usbIn (typeVal "current") :== Sum (
    (port p5vOut $ typeVal "current") :
    (port p3v3Out $ typeVal "current") :
    (map (\ gpio -> port gpio (typeVal "current")) gpios))

  endDef


















