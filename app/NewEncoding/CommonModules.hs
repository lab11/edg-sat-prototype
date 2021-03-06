module NewEncoding.CommonModules where

import Control.Monad

import EDG
import NewEncoding.CommonPorts
import NewEncoding.CommsPorts
import NewEncoding.Util

-- A button module
button :: Module ()
button = do
  setIdent "tactileSwitch"
  setSignature "tactileSwitch"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "button",
      "deviceData" <:= Record [
        "device" <:= StringV "tactileSwitch"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      -- for when button closes and resistor shorts to ground
      "current" <:= (range (FloatV 0.001) (FloatV 0.002)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()

  out <- addPort "out" $ do
    digitalSource
    setType [
      "voltage" <:= (range (FloatV 0) (FloatC unknown)),
      -- this resistor-switch topology doesn't allow current draw
      "current" <:= (range (FloatV 0) (FloatV 0)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0)),
      "0VoltageLevel" <:= FloatV 0,
      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, vin, out]

  setFieldsEq False [out, api] ["controlUid", "controlName"]
  setFieldsEq False [out, vin] ["voltage.max"]

  constrain $ port out (typeVal "1VoltageLevel") :== port vin (typeVal "voltage.min")

-- A LED Module
led :: Module ()
led = do
  setIdent "5mmLed"
  setSignature "5mmLed"
  -- setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "led",
      "deviceData" <:= Record [
        "device" <:= StringV "5mmLed"
        ]
      ]
    return ()

  source <- addPort "source" $ do
    digitalSink
    setType [
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36)),
      "limit0VoltageLevel" <:= FloatV 1.0,
      "limit1VoltageLevel" <:= FloatV 1.2,  -- TODO: voltage drops by LED color
      "current" <:= (range (FloatV 0.01) (FloatV 0.02)),
      "apiType" <:= StringV "onOff",  -- TODO: allow PWM
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, source]

  setFieldsEq True [api, source] [
      "controlUid"
    , "controlName"
    ]

  return ()

l7805 :: Module ()
l7805 = do
  setIdent "L7805 regulator"
  setSignature "L7805 regulator"
  setType [
    "powerDis" <:= FloatC unknown
    ]

  pwrIn <- addPort "pwrIn" $ do
    powerSink
    setType [
      "limitVoltage" <:= (range (FloatV 7) (FloatV 35))  -- TODO: use worst case 2.5v?
      ]
    return ()

  pwrOut <- addPort "pwrOut" $ do
    powerSource
    setType [
      "limitCurrent" <:= (range (FloatV 0) (FloatV 1.2)),
      "voltage" <:= (range (FloatV 4.8) (FloatV 5.2))
      ]
    return ()

  ensureConnected [pwrIn, pwrOut]
  constrain $ port pwrIn (typeVal "current.min") :== (port pwrOut (typeVal "current.min") :+ Lit (FloatV 6e-3))
  constrain $ port pwrIn (typeVal "current.max") :== (port pwrOut (typeVal "current.max") :+ Lit (FloatV 6e-3))
  constrain $ typeVal "powerDis" :==
    (port pwrIn (typeVal "current.max") :* (port pwrIn (typeVal "voltage.max") :- Lit (FloatV 5)))

apm3v3 :: Module ()
apm3v3 = do
  setIdent "Arduino Pro Micro 3v3"
  setSignature "Arduino Pro Micro 3v3"
  setType [
    "intCurrentMin" <:= FloatC unknown,
    "intCurrentMax" <:= FloatC unknown,
    "MHz" <:= FloatV 8.0  -- TODO: is this useful?
    ]

  usbIn <- addPort "usbDevice" $ do
    usbDevice
    setType [
        "limitVoltage" <:= (range (FloatV 4.5) (FloatV 5.5))
      ]
    return ()

  rawIn <- addPort "rawIn" $ do
    powerSink
    setType [  -- up to 600mV dropout
        "limitVoltage" <:= (range (FloatC unknown) (FloatV 12))
      ]
    return ()

  constrain $ (port usbIn connected) :|| (port rawIn connected)
  constrain $ Not ((port usbIn connected) :&& (port rawIn connected))

  constrain $ typeVal "intCurrentMin" :<= typeVal "intCurrentMax"
  constrain $ typeVal "intCurrentMin" :>= Lit (FloatV 0)

  constrain $ port usbIn connected :=>
    (port usbIn (typeVal "current.min") :== typeVal "intCurrentMin")
  constrain $ port usbIn connected :=>
    (port usbIn (typeVal "current.max") :== typeVal "intCurrentMax")

  constrain $ port rawIn connected :=>
    (port rawIn (typeVal "current.min") :== typeVal "intCurrentMin")
  constrain $ port rawIn connected :=>
    (port rawIn (typeVal "current.max") :== typeVal "intCurrentMax")

  constrain $ port rawIn (typeVal "current.max") :<= Lit (FloatV 0.5)  -- max regulator current rating
  -- Dropout calculated using linearization from two worst-case points: 300mV @ 150mA and 500mV @ 500mA
  constrain $ port rawIn (typeVal "limitVoltage.min")
    :== Lit (FloatV 3.514) :+ (Lit (FloatV 0.571) :* (port rawIn (typeVal "current.max")))

  p5vOut <- addPort "5vOut" $ do
    powerSource
    return ()

  setFieldsEq False [usbIn, p5vOut] ["voltage.max"]
  constrain $ (port p5vOut connected) :=> (port usbIn connected)

  -- MIC5219 regulator
  p3v3Out <- addPort "3v3Out" $ do
    powerSource
    setType [
      "voltage" <:= (range (FloatV 3.234) (FloatV 3.366))
      ]
    return ()

  gpios <- forM @[] [1..8] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalBidir
      setType [
        "limitCurrent" <:= (range (FloatV (-0.04)) (FloatV 0.04)),
        "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
        "0VoltageLevel" <:= FloatV 0,
        "apiDir" <:= StringV "producer"
        ]
      return ()

  digitalPins <- forM @[] ([0..10] ++ [14..16]) $ \ id ->
    newResource ("D" ++ (show id))
  analogPins <- forM @[] [0..3] $ \ id ->
    newResource ("A" ++ (show id))
  let pwmPins = map (\ pin -> digitalPins !! pin) [3, 5, 6, 9, 10]

  constrain $ typeVal "intCurrentMin" :== Sum (
    (port p5vOut $ typeVal "current.min") :
    (port p3v3Out $ typeVal "current.min") :
    (map (\ gpio -> port gpio (typeVal "current.min")) gpios))
  constrain $ typeVal "intCurrentMax" :== Sum (
    (port p5vOut $ typeVal "current.max") :
    (port p3v3Out $ typeVal "current.max") :
    (map (\ gpio -> port gpio (typeVal "current.max")) gpios))

  let constrainPortVoltageLevels setPort = do
          constrain $ port setPort (typeVal "limitVoltage.max"  )
            :== (port p3v3Out (typeVal "voltage.min") :+ Lit (FloatV 0.5))
          constrain $ port setPort (typeVal "limit0VoltageLevel")
            :== (port p3v3Out (typeVal "voltage.min") :* Lit (FloatV 0.2) :+ Lit (FloatV 0.1))
          constrain $ port setPort (typeVal "limit1VoltageLevel")
            :== (port p3v3Out (typeVal "voltage.max") :* Lit (FloatV 0.2) :+ Lit (FloatV 0.9))
          constrain $ port setPort (typeVal "1VoltageLevel")
            :== port p3v3Out (typeVal "voltage.min")

  forM_ @[] gpios $ \ gpio -> do
    let isSource = port gpio (typeVal "digitalDir") :== Lit (StringV "source")

    constrain $ port gpio (typeVal "controlUid") :== uid
    constrain $ isSource :=> port gpio (typeVal "voltage.min") :== Lit (FloatV 0)
    constrain $ isSource :=> port gpio (typeVal "voltage.max") :== port p3v3Out (typeVal "voltage.max")

    constrainResources (gpio ++ "dig") (port gpio connected :&& port gpio (typeVal "apiType") :== Lit (StringV "onOff"))
      [gpio :|= (digitalPins ++ analogPins)]
    constrainResources (gpio ++ "pwm") (port gpio connected :&& port gpio (typeVal "apiType") :== Lit (StringV "pwm"))
      [gpio :|= (pwmPins)]

    constrainPortVoltageLevels gpio

  analogs <- forM @[] [0..3] $ \ analogId ->
    addPort ("analog" ++ (show analogId)) $ do
      analogSink
      setType [
        "current" <:= range (FloatV 0) (FloatV 0),  -- a simplistic model...
        "limitVoltage" <:= range (FloatV (-0.5)) (FloatC unknown),
        "limitScale" <:= range (FloatV 0) (FloatC unknown),
        "limitBits" <:= FloatV 10,
        "apiDir" <:= StringV "producer"
        ]
      return ()

  forM @[] analogs $ \ analog -> do
    constrain $ port analog (typeVal "controlUid") :== uid
    constrain $ port analog (typeVal "limitVoltage.max") :== (port p3v3Out (typeVal "voltage.min") :+ Lit (FloatV 0.5))
    constrain $ port analog (typeVal "limitScale.max") :== port p3v3Out (typeVal "voltage.max")

    constrainResources analog (port analog $ connected) [analog :|= analogPins]

  i2c <- addPort "i2c" $ do
    i2cMaster
    setType [
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
      "0VoltageLevel" <:= FloatV 0,
      "frequency" <:= range (FloatV 0) (FloatV 400e3)
      ]
    return ()

  constrain $ port i2c (typeVal "controlUid") :== uid

  constrainPortVoltageLevels i2c

  constrainResources i2c (port i2c $ connected) [
    (i2c ++ "SDA") :|= [digitalPins !! 2],
    (i2c ++ "SCL") :|= [digitalPins !! 3]
    ]

  uart <- addPort "uart" $ do
    uartMaster
    setType [
      "voltage" <:= (range (FloatV 0) (FloatC unknown)),
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
      "0VoltageLevel" <:= FloatV 0,
      "baud" <:= range (FloatV 0) (FloatV 1e6)
      ]
    return ()

  setFieldsEq False [uart, p3v3Out] ["voltage.max"]

  constrain $ port uart (typeVal "controlUid") :== uid

  constrainPortVoltageLevels uart

  constrainResources uart (port uart $ connected) [
    (uart ++ "TX") :|= [digitalPins !! 1],
    (uart ++ "RX") :|= [digitalPins !! 0]
    ]

  spi <- addPort "spi" $ do
    spiMaster
    setType [
      "voltage" <:= (range (FloatV 0) (FloatC unknown)),
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
      "0VoltageLevel" <:= FloatV 0,
      "frequency" <:= range (FloatV 0) (FloatV 4e6)  -- max of fOsc/2
      ]
    return ()

  setFieldsEq False [spi, p3v3Out] ["voltage.max"]

  constrain $ port spi (typeVal "controlUid") :== uid

  constrainPortVoltageLevels spi

  constrainResources spi (port spi $ connected) [
    (spi ++ "SCK" ) :|= [digitalPins !! 12],
    (spi ++ "MISO") :|= [digitalPins !! 11],
    (spi ++ "MOSI") :|= [digitalPins !! 13]
    ]

  return ()
