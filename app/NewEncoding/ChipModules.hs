module NewEncoding.ChipModules where

import EDG
import NewEncoding.CommonPorts
import NewEncoding.CommonModules
import NewEncoding.CommsPorts

-- A LED Module
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
      "limitVoltage" <:= (range (FloatV (- 0.5)) (FloatV 3.6)),
      "lowVoltage" <:= FloatV 0.4,
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
