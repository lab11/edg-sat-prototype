module NewEncoding.RedundantModules where

import Control.Monad

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonModules
import NewEncoding.CommsPorts

domeButton :: Module ()
domeButton = do
  setIdent "domeButton"
  setSignature "domeButton"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "litButton",
      "deviceData" <:= Record [
        "device" <:= StringV "domeButton"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      -- for when button closes and resistor shorts to ground
      "current" <:= range (FloatV 0.001) (FloatV 0.002),
      "limitVoltage" <:= range (FloatV 0) (FloatV 36)
      ]
    return ()

  button <- addPort "button" $ do
    digitalSource
    setType [
      "voltage" <:= range (FloatV 0) (FloatC unknown),
      -- this resistor-switch topology doesn't allow current draw
      "current" <:= range (FloatV 0) (FloatV 0),
      "limitCurrent" <:= range (FloatV 0) (FloatV 0),
      "0VoltageLevel" <:= FloatV 0,
      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  light <- addPort "light" $ do
    digitalSink
    setType [
      "limitVoltage" <:= range (FloatV 0) (FloatV 13),
      "limit0VoltageLevel" <:= FloatV 1.0,
      "limit1VoltageLevel" <:= FloatV 11,  -- TODO: voltage drops by LED color
      "current" <:= range (FloatV 0.01) (FloatV 0.02),
      "apiType" <:= StringV "onOff",  -- TODO: allow PWM
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, vin, button, light]

  setFieldsEq False [api, button, light] ["controlUid", "controlName"]
  setFieldsEq False [vin, button] ["voltage.max"]

  constrain $ port button (typeVal "1VoltageLevel") :== port vin (typeVal "voltage.min")


openLog :: Module ()
openLog = do
  setIdent "Sparkfun OpenLog"
  setSignature "Sparkfun OpenLog"

  setType [
    "regVoltage" <:= range (FloatC unknown) (FloatC unknown)
    ]

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "fat32",
      "apiData" <:= Record [
        "size" <:= IntV 68719476736,  -- 64 GiB
        "tech" <:= StringV "flash",
        "form" <:= StringV "SDCard"
        ],
      "deviceData" <:= Record [
        "device" <:= StringV "openlog"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= range (FloatV 0) (FloatV 6e-3),
      -- SD card requires at least 2.7v, while ATmega328 operates down to 1.8v
      -- Top of range is as recommended by Sparkfun
      "limitVoltage" <:= range (FloatV 2.865) (FloatV 5.5)
      ]
    return ()

  constrain $ typeVal "regVoltage.min" :== If (port vin (typeVal "voltage.min") :>= Lit (FloatV 3.465))
      (Lit (FloatV 3.3))
      (port vin (typeVal "voltage.min") :- Lit (FloatV 0.165))
  constrain $ typeVal "regVoltage.max" :== If (port vin (typeVal "voltage.max") :>= Lit (FloatV 3.465))
      (Lit (FloatV 3.3))
      (port vin (typeVal "voltage.max") :- Lit (FloatV 0.165))

  uart <- addPort "uart" $ do
    uartSlave
    setType [
      "voltage" <:= range (FloatV 0) (FloatC unknown),

      "limitVoltage" <:= range (FloatV (-0.5)) (FloatC unknown),
      "0VoltageLevel" <:= FloatV 0.6,
      "1VoltageLevel" <:= FloatV 2.3,

      "baud" <:= range (FloatV 0) (FloatV 115200)  -- "up to 115200bps"
      ]
    return ()

  ensureConnected [api, vin, uart]

  constrain $ port uart (typeVal "voltage.max") :== typeVal "regVoltage.max"
  constrain $ port uart (typeVal "limitVoltage.max") :== typeVal "regVoltage.min" :+ Lit (FloatV 0.5)

  constrain $ port uart (typeVal "limit0VoltageLevel") :== typeVal "regVoltage.min" :* Lit(FloatV 0.3)
  constrain $ port uart (typeVal "limit1VoltageLevel") :== typeVal "regVoltage.max" :* Lit (FloatV 0.6)

  setFieldsEq False [uart, api] ["controlUid", "controlName"]

  return ()


arduinoTrinket3v3 :: Module ()
arduinoTrinket3v3 = do
  setIdent "Arduino Trinket 3.3v"
  setSignature "Arduino Trinket 3.3v"
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
        "limitVoltage" <:= (range (FloatV 3.75) (FloatV 16))
      ]
    return ()

  constrain $ (port usbIn connected) :|| (port rawIn connected)
  constrain $ Not ((port usbIn connected) :&& (port rawIn connected))

  constrain $ port usbIn connected :=>
    (port usbIn (typeVal "current.min") :== typeVal "intCurrentMin")
  constrain $ port usbIn connected :=>
    (port usbIn (typeVal "current.max") :== typeVal "intCurrentMax")

  constrain $ port rawIn connected :=>
    (port rawIn (typeVal "current.min") :== typeVal "intCurrentMin")
  constrain $ port rawIn connected :=>
    (port rawIn (typeVal "current.max") :== typeVal "intCurrentMax")

  constrain $ port rawIn (typeVal "current.max") :<= Lit (FloatV 0.15)  -- max regulator current rating

  p5vOut <- addPort "5vOut" $ do
    powerSource
    return ()

  setFieldsEq False [usbIn, p5vOut] ["voltage.max"]
  constrain $ (port p5vOut connected) :=> (port usbIn connected)

  -- MIC5225 regulator
  p3v3Out <- addPort "3v3Out" $ do
    powerSource
    setType [
      "voltage" <:= (range (FloatV 3.234) (FloatV 3.366)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0.150))
      ]
    return ()

  gpios <- forM @[] [1..3] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalBidir
      setType [
        "limitCurrent" <:= (range (FloatV (-0.04)) (FloatV 0.04)),
        "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
        "0VoltageLevel" <:= FloatV 0.5,
        "1VoltageLevel" <:= FloatV 2.5
        ]
      return ()

  pins <- forM @[] ([0..2]) $ \ id ->
    newResource ("Pin" ++ (show id))

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
            :== (port p3v3Out (typeVal "voltage.min") :* Lit (FloatV 0.3))
          constrain $ port setPort (typeVal "limit1VoltageLevel")
            :== (port p3v3Out (typeVal "voltage.max") :* Lit (FloatV 0.6))

  forM_ @[] gpios $ \ gpio -> do
    let isSource = port gpio (typeVal "digitalDir") :== Lit (StringV "source")

    constrain $ port gpio (typeVal "controlUid") :== uid
    constrain $ isSource :=> port gpio (typeVal "voltage.min") :== Lit (FloatV 0)
    constrain $ isSource :=> port gpio (typeVal "voltage.max") :== port p3v3Out (typeVal "voltage.max")

    constrainResources (gpio ++ "dig") (port gpio connected :&& port gpio (typeVal "apiType") :== Lit (StringV "onOff"))
      [gpio :|= (pins)]
    constrainResources (gpio ++ "pwm") (port gpio connected :&& port gpio (typeVal "apiType") :== Lit (StringV "pwm"))
      [gpio :|= [pins !! 0, pins !! 1]]

    constrainPortVoltageLevels gpio

  i2c <- addPort "i2c" $ do
    i2cMaster
    setType [
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
      "0VoltageLevel" <:= FloatV 0.5,
      "frequency" <:= range (FloatV 0) (FloatV 400e3)
      ]
    return ()

  constrain $ port i2c (typeVal "controlUid") :== uid

  constrainPortVoltageLevels i2c

  constrainResources i2c (port i2c $ connected) [
    (i2c ++ "SDA") :|= [pins !! 0],
    (i2c ++ "SCL") :|= [pins !! 2]
    ]

  -- device has no UART pins exposed
  -- device has a SPI interface, but insufficient pins for even a single CS

  -- TODO: add ADC interfaces

  return ()
