module NewEncoding.ChipModules where

import Control.Monad

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonModules
import NewEncoding.CommsPorts

tmp102 :: Module ()
tmp102 = do
  setIdent "TMP102 Temp Sensor"
  setSignature "TMP102 Temp Sensor"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "temperatureSensor",
      -- TODO: more temp sensor properties, scale resolution accuracy
      "apiData" <:= Record [
        "tempRange" <:= (range (FloatV (-25)) (FloatV 85)),
        "tempAccuracy" <:= FloatV 2,
        "tempResolution" <:= FloatV 0.0625
        ],
      "deviceData" <:= Record [
        "device" <:= StringV "tmp102"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "limitVoltage" <:= (range (FloatV 2.0) (FloatV 3.6))  -- TODO technically down to 1.4 for digital thresholds characterized for V+>2.0
      ]
    return ()

  i2c <- addPort "i2c" $ do
    i2cSlave
    setType [
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatV 3.6)),
      "0VoltageLevel" <:= FloatV 0.4,
      "frequency" <:= range (FloatV 1e3) (FloatV 3.4e6),
      "id" <:= IntC $ oneOf[72, 73, 74, 75]
      ]
    return ()

  ensureConnected [api, vin, i2c]

  constrain $ port i2c (typeVal "limit0VoltageLevel") :== (port vin (typeVal "voltage.min") :* Lit (FloatV 0.3))
  constrain $ port i2c (typeVal "limit1VoltageLevel") :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.7))

  setFieldsEq False [i2c, api] ["controlUid", "controlName"]

  return ()

-- Common properties fr Sparkfun Serial LCDs with a PIC16F88
serialLcdBase16f88 :: Module (PortName,PortName)
serialLcdBase16f88 = do
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "characterLcd"
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 100e-3))  -- guesstimate, not specified
      ]
    return ()

  uart <- addPort "uart" $ do
    uartSlave
    setType [
      "voltage" <:= (range (FloatV 0) (FloatV 0)),  -- no output

      "limitVoltage" <:= (range (FloatV (-0.3)) (FloatC unknown)),
      -- "0VoltageLevel" <:= FloatV 0.6,
      -- doesn't transmit, make it compatible with everything
      "0VoltageLevel" <:= FloatV 0,
      "baud" <:= range (FloatV 2400) (FloatV 38400)  -- limitation of SFE Serial LCD, not PIC16F88
      ]
    return ()

  ensureConnected [api, vin, uart]

  constrain $ port uart (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))

  -- constrain $ port "uart" (typeVal "1VoltageLevel") :== (port vin (typeVal "voltage.min") :- Lit (FloatV 0.7))
  -- doesn't transmit, make it compatible with everything
  constrain $ port uart (typeVal "1VoltageLevel") :== port vin (typeVal "voltage.min")

  -- datasheet isn't clear about the RX input type, assuming TTL
  constrain $ port uart (typeVal "limit0VoltageLevel") :== (port vin (typeVal "voltage.min") :* Lit(FloatV 0.15))
  constrain $ port uart (typeVal "limit1VoltageLevel") :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.25) :+ Lit (FloatV 0.8))

  setFieldsEq False [uart, api] ["controlUid", "controlName"]

  return (api,vin)

serialLcd16x2_3v3 :: Module ()
serialLcd16x2_3v3 = do
  (api, vin) <- serialLcdBase16f88

  setIdent "Sparkfun Serial LCD 16x2 3.3v"
  setSignature "Sparkfun Serial LCD 16x2 3.3v"

  constrain $ port api (typeVal "deviceData") :== Lit (Record [
    "device" <:= StringV "SparkfunSerialLcd16x2_3v3"
    ])
  constrain $ port api (typeVal "apiData") :== Lit (Record [
    "width" <:= IntV 16,
    "height" <:= IntV 2
    ])
  constrain $ port vin (typeVal "limitVoltage") :== Lit (range (FloatV 3.0) (FloatV 3.6))

  return ()

serialLcd16x2_5v :: Module ()
serialLcd16x2_5v = do
  (api, vin) <- serialLcdBase16f88

  setIdent "Sparkfun Serial LCD 16x2 5v"
  setSignature "Sparkfun Serial LCD 16x2 5v"

  constrain $ port api (typeVal "deviceData") :== Lit (Record [
    "device" <:= StringV "SparkfunSerialLcd16x2_5v"
    ])
  constrain $ port api (typeVal "apiData") :== Lit (Record [
    "width" <:= IntV 16,
    "height" <:= IntV 2
    ])
  constrain $ port vin (typeVal "limitVoltage") :== Lit (range (FloatV 4.5) (FloatV 5.5))

  return ()


sdcard :: Module ()
sdcard = do
  setIdent "SD Card"
  setSignature "SD Card"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "nvmemory",
      -- TODO: more temp sensor properties, scale resolution accuracy
      "apiData" <:= Record [
        "size" <:= IntV 137438953472,  -- 128 GiB
        "tech" <:= StringV "flash",
        "form" <:= StringV "SDCard"
        ],
      "deviceData" <:= Record [
        "device" <:= StringV "sdcard"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= range (FloatV 0) (FloatV 200e-6),
      "limitVoltage" <:= range (FloatV 2.7) (FloatV 3.6)
      ]
    return ()

  spi <- addPort "spi" $ do
    spiSlave
    setType [
      "voltage" <:= range (FloatV 0) (FloatC unknown),
      "limitVoltage" <:= range (FloatV (-0.3)) (FloatC unknown),

      -- actual logic level thresholds are unknown, not part of the simplified spec
      "0VoltageLevel" <:= FloatV 0,
      "limit1VoltageLevel" <:= FloatV 0,
      "mode" <:= IntV 0,
      "frequency" <:= (range (FloatV 0) (FloatV 25e6))  -- base spec SD cards only up to 25MHz
      ]
    return ()

  cs <- addPort "cs" $ do
    digitalSink
    setType [
      "limitVoltage" <:= range (FloatV (-0.3)) (FloatC unknown),
      -- actual logic level thresholds are unknown, not part of the simplified spec

      "limit1VoltageLevel" <:= FloatV 0,

      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, vin, spi, cs]

  setFieldsEq False [spi, vin] ["voltage.max"]
  setFieldsEq False [spi, cs, api] ["controlUid", "controlName"]

  -- actual max voltage limits also unknown, assuming +/-0.3v
  constrain $ port spi (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))
  constrain $ port cs (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))

  -- actual logic level thresholds are unknown, not part of the simplified spec
  constrain $ port spi (typeVal "limit0VoltageLevel") :== port vin (typeVal "voltage.min")
  constrain $ port spi (typeVal "limit0VoltageLevel") :== port vin (typeVal "voltage.min")
  constrain $ port spi (typeVal "1VoltageLevel")      :== port vin (typeVal "voltage.min")
  constrain $ port cs (typeVal "limit0VoltageLevel")  :== port vin (typeVal "voltage.min")

  return ()

pcf8575 :: Module ()
pcf8575 = do
  setIdent "PCF8575 I2C Expander"
  setSignature "PCF8575 I2C Expander"
  setType []

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "limitVoltage" <:= (range (FloatV 2.5) (FloatV 5.5))
      ]
    return ()

  let constrainPortVoltageLimits setPort = do
          constrain $ port setPort (typeVal "limitVoltage.max"  )
            :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.5))
          constrain $ port setPort (typeVal "limit0VoltageLevel")
            :== (port vin (typeVal "voltage.min") :* Lit (FloatV 0.3))
          constrain $ port setPort (typeVal "limit1VoltageLevel")
            :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.7))

  i2c <- addPort "i2c" $ do
    i2cSlave
    setType [
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
      "0VoltageLevel" <:= FloatV 0.4,  -- guess based on specified Vol test conditions for SDA Iol
      "frequency" <:= range (FloatV 0) (FloatV 400e3),
      "id" <:= IntC $ oneOf[20, 21, 22, 23, 24, 25, 26, 27],
      "controlName" <:= StringV "pcf8575"
      ]
    return ()

  ensureConnected [vin, i2c]

  constrain $ port i2c (typeVal "limit0VoltageLevel") :== (port vin (typeVal "voltage.min") :* Lit (FloatV 0.3))
  constrain $ port i2c (typeVal "limit1VoltageLevel") :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.7))
  constrain $ port i2c (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.5))

  -- Technically, this thing has 16 GPIOs, but is overkill and impacts performance
  gpios <- forM @[] [1..8] $ \ gpioId ->
    addPort ("gpio" ++ (show gpioId)) $ do
      digitalBidir
      setType [
        "limitCurrent" <:= (range (FloatV (-1e-3)) (FloatV 25e-3)),
        "limitVoltage" <:= (range (FloatV (-0.5)) (FloatC unknown)),
        "0VoltageLevel" <:= FloatV 0.5,
        "1VoltageLevel" <:= FloatV 2.3,
        "apiType" <:= StringV "onOff",
        "apiDir" <:= StringV "producer"
        ]
      return ()

  forM_ @[] gpios $ \ gpio -> do
    let isSource = port gpio (typeVal "digitalDir") :== Lit (StringV "source")
    constrain $ isSource :=> port gpio (typeVal "voltage.min") :== Lit (FloatV 0)
    constrain $ isSource :=> port gpio (typeVal "voltage.max") :== port vin (typeVal "voltage.max")
    constrainPortVoltageLimits gpio

  constrain $ Any (map (\ gpio -> port gpio connected) gpios)

  constrain $ port vin (typeVal "current.min") :== Sum (
    (Lit (FloatV 100e-6)) :  -- device operating current
    (map (\ gpio -> port gpio (typeVal "current.min")) gpios))
  constrain $ port vin (typeVal "current.max") :== Sum (
    (Lit (FloatV 200e-6)) :  -- device operating current
    (map (\ gpio -> port gpio (typeVal "current.max")) gpios))

  setFieldsEq False (i2c : gpios) ["controlUid"]

  return ()



pwmControlFan :: Module ()
pwmControlFan = do
  setIdent "PWM Signal 12V fan"
  setSignature "PWM Signal 12V fan"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "controlledFan",
      -- TODO: more properties
      "apiData" <:= Record [
        ],
      "deviceData" <:= Record [
        "device" <:= StringV "pwmSignalFan"
        ]
      ]
    return ()

  -- spec from http://www.formfactors.org/developer/specs/4_wire_pwm_spec.pdf
  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 1.5)),
      "limitVoltage" <:= (range (FloatV 11.4) (FloatV 12.6))
      ]
    return ()


  control <- addPort "control" $ do
    -- open-drain input, pull-up within the fan
    digitalSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 5e-3)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 5.25)),
      "limit0VoltageLevel" <:= FloatV 0.8,
      "limit1VoltageLevel" <:= FloatV 0,  -- don't care for open-drain
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, vin, control]

  return ()


powerControlFan :: Module ()
powerControlFan = do
  setIdent "Power Controlled 12V fan"
  setSignature "Power Controlled 12V fan"
  setType []

  api <- addPort "api" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "controlledFan",
      -- TODO: more properties
      "apiData" <:= Record [
        ],
      "deviceData" <:= Record [
        "device" <:= StringV "voltageControlFan"
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    digitalSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 1.5)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 12.6)),
      "limit0VoltageLevel" <:= FloatV 3.0,  -- TODO non guesstimate ratings
      "limit1VoltageLevel" <:= FloatV 11.4,
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  ensureConnected [api, vin]

  return ()


-- TODO: get current rating, level stats from a actual MOSFET
-- TODO: constraint to avoid inferring unnecessary amps
digitalAmplifier :: Module ()
digitalAmplifier = do
  setIdent "digitalAmplifier"
  setSignature "digitalAmplifier"
  setType []

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "limitVoltage" <:= (range (FloatV 0) (FloatV 60))  -- Vds rating
      ]
    return ()

  control <- addPort "control" $ do
    digitalSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0)),  -- no current draw into FET gate
      "limitVoltage" <:= (range (FloatV 0) (FloatV 20)),  -- Vgs rating
      "limit0VoltageLevel" <:= FloatV 1.0,
      "limit1VoltageLevel" <:= FloatV 2.0,  -- logic level gate MOSFET
      "apiType" <:= StringV "onOff",  -- only allow low frequency drive, for now
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  out <- addPort "out" $ do
    digitalSource
    setType [
      "voltage" <:= range (FloatV 0) (FloatC unknown),
      "limitCurrent" <:= range (FloatV 0) (FloatV 10),
      "0VoltageLevel" <:= FloatV 0,
      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "producer"
      ]
    return ()

  ensureConnected [vin, control, out]

  setFieldsEq False [control, out] ["controlUid", "controlName"]
  setFieldsEq False [out, vin] ["voltage.max"]
  setFieldsEq False [out, vin] ["current"]  -- ignoring gate leakage and pullup resistor for now

  constrain $ port out (typeVal "1VoltageLevel") :== port vin (typeVal "voltage.min")
