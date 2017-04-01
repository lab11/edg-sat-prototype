module NewEncoding.ChipModules where

import EDG
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
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0.5e-6) (FloatV 85e-6)),
      "limitVoltage" <:= (range (FloatV 2.0) (FloatV 3.6))  -- TODO technically down to 1.4 for digital thresholds characterized for V+>2.0
      ]
    return ()

  i2c <- addPort "i2c" $ do
    i2cSlave
    setType [
      "limitCurrent" <:= (range (FloatV 0) (FloatV 1e-6)),
      "limitVoltage" <:= (range (FloatV (-0.5)) (FloatV 3.6)),
      "lowVoltage" <:= FloatV 0.4,
      "frequency" <:= range (FloatV 1e3) (FloatV 3.4e6),
      "id" <:= IntC $ oneOf[72, 73, 74, 75]
      ]
    return ()

  constrain $ port api connected
  constrain $ port vin connected
  constrain $ port i2c connected

  constrain $ port i2c (typeVal "limitLowVoltage") :== (port vin (typeVal "voltage.min") :* Lit (FloatV 0.3))
  constrain $ port i2c (typeVal "limitHighVoltage") :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.7))

  constrain $ port i2c (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port i2c (typeVal "controlName") :== port api (typeVal "controlName")

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
      "current" <:= (range (FloatV 0) (FloatV 0)),

      "limitCurrent" <:= (range (FloatV (-25e-3)) (FloatV 25e-3)),
      "limitVoltage" <:= (range (FloatV (-0.3)) (FloatC unknown)),
      -- "lowVoltage" <:= FloatV 0.6,
      -- doesn't transmit, make it compatible with everything
      "lowVoltage" <:= FloatV 0,
      "baud" <:= range (FloatV 2400) (FloatV 38400)  -- limitation of SFE Serial LCD, not PIC16F88
      ]
    return ()

  constrain $ port api connected
  constrain $ port vin connected
  constrain $ port uart connected

  constrain $ port uart (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))

  -- constrain $ port "uart" (typeVal "highVoltage") :== (port vin (typeVal "voltage.min") :- Lit (FloatV 0.7))
  -- doesn't transmit, make it compatible with everything
  constrain $ port uart (typeVal "highVoltage") :== port vin (typeVal "voltage.min")

  -- datasheet isn't clear about the RX input type, assuming TTL
  constrain $ port uart (typeVal "limitLowVoltage") :== (port vin (typeVal "voltage.min") :* Lit(FloatV 0.15))
  constrain $ port uart (typeVal "limitHighVoltage") :== (port vin (typeVal "voltage.max") :* Lit (FloatV 0.25) :+ Lit (FloatV 0.8))

  constrain $ port uart (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port uart (typeVal "controlName") :== port api (typeVal "controlName")

  return (api,vin)

serialLcd16x2_3v3 :: Module ()
serialLcd16x2_3v3 = do
  (api, vin) <- serialLcdBase16f88

  setIdent "Sparkfun Serial LCD 16x2 3.3v"
  setSignature "Sparkfun Serial LCD 16x2 3.3v"

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
        ]
      ]
    return ()

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 200e-6)),
      "limitVoltage" <:= (range (FloatV 2.7) (FloatV 3.6))
      ]
    return ()

  spi <- addPort "spi" $ do
    spiSlave
    setType [
      "voltage" <:= (range (FloatV 0) (FloatC unknown)),
      "limitVoltage" <:= (range (FloatV (-0.3)) (FloatC unknown)),

      "current" <:= (range (FloatV 0) (FloatV 0)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0)),
      -- actual logic level thresholds are unknown, not part of the simplified spec
      "lowVoltage" <:= FloatV 0,
      "limitHighVoltage" <:= FloatV 0,
      "mode" <:= IntV 0,
      "frequency" <:= (range (FloatV 0) (FloatV 25e6))  -- base spec SD cards only up to 25MHz
      ]
    return ()

  cs <- addPort "cs" $ do
    digitalSink
    setType [
      "limitVoltage" <:= (range (FloatV (-0.3)) (FloatC unknown)),
      "current" <:= (range (FloatV 0) (FloatV 0)),
      -- actual logic level thresholds are unknown, not part of the simplified spec

      "limitHighVoltage" <:= FloatV 0,

      "apiType" <:= StringV "onOff",
      "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port api connected
  constrain $ port vin connected
  constrain $ port spi connected
  constrain $ port cs connected

  constrain $ port spi (typeVal "voltage.max") :== port vin (typeVal "voltage.max")

  -- actual max voltage limits also unknown, assuming +/-0.3v
  constrain $ port spi (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))
  constrain $ port cs (typeVal "limitVoltage.max") :== (port vin (typeVal "voltage.min") :+ Lit (FloatV 0.3))

  -- actual logic level thresholds are unknown, not part of the simplified spec
  constrain $ port spi (typeVal "limitLowVoltage") :== port vin (typeVal "voltage.min")
  constrain $ port spi (typeVal "limitLowVoltage") :== port vin (typeVal "voltage.min")
  constrain $ port spi (typeVal "highVoltage") :== port vin (typeVal "voltage.min")
  constrain $ port cs (typeVal "limitLowVoltage") :== port vin (typeVal "voltage.min")

  constrain $ port spi (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port spi (typeVal "controlName") :== port api (typeVal "controlName")
  constrain $ port cs (typeVal "controlUid") :== port api (typeVal "controlUid")
  constrain $ port cs (typeVal "controlName") :== port api (typeVal "controlName")

  return ()
