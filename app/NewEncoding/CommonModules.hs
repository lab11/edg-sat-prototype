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

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "button"
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= FloatV 0.001  -- for when button closes and resistor shorts to ground
      ]
    return ()
  out <- addPort "out" $ do
    digitalSource
    setType [
      "current" <:= FloatV 0,  -- this resistor-switch topology doesn't allow current draw
      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port api connected
  constrain $ port vin connected
  constrain $ port out connected

  constrain $ port out (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port out (typeVal "controlName") :== port api (typeVal "controlName")

  constrain $ port vin (typeVal "voltage") :== port out (typeVal "voltage")
  -- TODO digital signal threhsolds

-- A LED Module
led :: Module ()
led = do
  setIdent "led"
  setSignature "led"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "led"
      ]
    return ()

  source <- addPort "source" $ do
    digitalSource
    setType [
      "current" <:= FloatV 0.01,
      "apiType" <:= StringV "onOff",  -- TODO: allow PWM
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port api connected
  constrain $ port source connected

  constrain $ port source (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port source (typeVal "controlName") :== port api (typeVal "controlName")
  --constrain $ port source (typeVal "voltage") :>= Lit (FloatV 3.0)  -- LED voltage drop

  -- TODO digital signal threhsolds

mcu :: Module ()
mcu = do
  setIdent "Microcontroller"
  setSignature "Microcontroller"
  setType [
    "MHz" <:= FloatV 8.0  -- TODO: is this useful?
    ]

  usbIn <- addPort "UsbIn" $ do
    powerSink
    setType [
      "voltage" <:= FloatV 5.0,
      "limitVoltage" <:= FloatV 5.5,
      "current" <:= FloatC (lessThan 0.5)
      ]
    return ()
  constrain $ Not (port usbIn connected)  -- dummy port only

  p5vOut <- addPort "5vOut" $ do
    powerSource
    setType [
      ]
    return ()
  constrain $ port p5vOut (typeVal "voltage") :== port usbIn (typeVal "voltage")


  p3v3Out <- addPort "3v3Out" $ do
    powerSource
    setType [
      "voltage" <:= FloatV 3.3
      ]
    return ()

  gpios <- forM @[] [1..4] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalBidir
      setType [
        "voltage" <:= FloatV 3.3,
        "limitCurrent" <:= FloatV 0.04
        ]
      return ()

  constrain $ port usbIn (typeVal "current") :== Sum (
    (port p5vOut $ typeVal "current") :
    (port p3v3Out $ typeVal "current") :
    (map (\ gpio -> port gpio (typeVal "current")) gpios))

  endDef


















