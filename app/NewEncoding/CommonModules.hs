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
      "current" <:= (range (FloatV 0.001) (FloatV 0.002)),  -- for when button closes and resistor shorts to ground
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()
  out <- addPort "out" $ do
    digitalSource
    setType [
      "voltage" <:= (range (FloatV 0) (FloatC unknown)),
      "current" <:= (range (FloatV 0) (FloatV 0)),  -- this resistor-switch topology doesn't allow current draw
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0)),  -- this resistor-switch topology doesn't allow current draw
      "lowVoltage" <:= FloatV 0,
      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port api connected
  constrain $ port vin connected
  constrain $ port out connected

  constrain $ port out (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port out (typeVal "controlName") :== port api (typeVal "controlName")

  constrain $ port out (typeVal "voltage.max") :== port vin (typeVal "voltage.max")
  constrain $ port out (typeVal "highVoltage") :== port vin (typeVal "voltage.min")

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
    digitalSink
    setType [
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36)),
      "limitLowVoltage" <:= FloatV 1.0,
      "limitHighVoltage" <:= FloatV 1.2,  -- TODO: voltage drops by LED color
      "current" <:= (range (FloatV 0.01) (FloatV 0.02)),
      "apiType" <:= StringV "onOff",  -- TODO: allow PWM
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port api connected
  constrain $ port source connected

  constrain $ port source (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port source (typeVal "controlName") :== port api (typeVal "controlName")

mcu :: Module ()
mcu = do
  setIdent "Arduino Pro Micro 3v3"
  setSignature "Arduino Pro Micro 3v3"
  setType [
    "MHz" <:= FloatV 8.0  -- TODO: is this useful?
    ]

  -- TODO add USBIn port and constraint

  p5vOut <- addPort "5vOut" $ do
    powerSource
    setType [
      "voltage" <:= (range (FloatV 4.5) (FloatV 5.5)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0.5))
      ]
    return ()

  -- MIC5219 regulator
  p3v3Out <- addPort "3v3Out" $ do
    powerSource
    setType [
      "voltage" <:= (range (FloatV 3.234) (FloatV 3.366)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0.5))
      ]
    return ()

  gpios <- forM @[] [1..10] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalBidir
      setType [
        "limitCurrent" <:= (range (FloatV (-0.04)) (FloatV 0.04)),
        "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown))
        ]
      return ()

  digitalPins <- forM @[] ([0..10] ++ [4..16]) $ \ id ->
    newResource ("D" ++ (show id))
  analogPins <- forM @[] [0..3] $ \ id ->
    newResource ("A" ++ (show id))

-- TODO: add current sums again
--  constrain $ port usbIn (typeVal "current.min") :== Sum (
--    (port p5vOut $ typeVal "current.min") :
--    (port p3v3Out $ typeVal "current.min") :
--    (map (\ gpio -> port gpio (typeVal "current.min")) gpios))
--  constrain $ port usbIn (typeVal "current.max") :== Sum (
--    (port p5vOut $ typeVal "current.max") :
--    (port p3v3Out $ typeVal "current.max") :
--    (map (\ gpio -> port gpio (typeVal "current.max")) gpios))

  forM @[] gpios $ \ gpio -> do
    let isSource = port gpio (typeVal "digitalDir") :== Lit (StringV "source")

    constrain $ port gpio (typeVal "controlUid") :== uid
    constrain $ isSource :=> port gpio (typeVal "voltage") :== port p3v3Out (typeVal "voltage")
    constrain $ port gpio (typeVal "limitVoltage.max") :== (port p3v3Out (typeVal "voltage.min") :+ Lit (FloatV 0.5))

    constrain $ isSource :=> (port gpio (typeVal "lowVoltage") :== Lit (FloatV 0.5))
    constrain $ isSource :=> (port gpio (typeVal "highVoltage") :== Lit (FloatV 2.3))
    constrain $ port gpio (typeVal "limitLowVoltage") :== (port p3v3Out (typeVal "voltage.min") :* Lit (FloatV 0.2) :+ Lit (FloatV 0.1))
    constrain $ port gpio (typeVal "limitHighVoltage") :== (port p3v3Out (typeVal "voltage.max") :* Lit (FloatV 0.2) :+ Lit (FloatV 0.9))

    constrainResources gpio (port gpio $ connected) [gpio :|= (digitalPins ++ analogPins)]

  endDef
