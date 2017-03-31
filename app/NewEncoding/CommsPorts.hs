module NewEncoding.CommsPorts where

import Control.Monad

import EDG
import NewEncoding.CommonPorts

spiBase :: (IsPort p) => p ()
spiBase = do
  digitalBidirBase
  setType [
    "frequency" <:= FloatC unknown
    ]
  return ()

spiMaster :: (IsPort p) => p ()
spiMaster = do
  spiBase
  setKind "SpiMaster"
  setIdent "SpiMaster"
  return ()

spiSlave :: (IsPort p) => p ()
spiSlave = do
  spiBase
  setKind "SpiSlave"
  setIdent "SpiSlave"
  setType [
    "mode" <:= IntC $ oneOf [0, 1, 2, 3]
    ]
  return ()

uartBase :: (IsPort p) => p ()
uartBase = do
  digitalBidirBase
  setType [
    "uart" <:= FloatC unknown
    ]
  return ()

uartMaster :: (IsPort p) => p ()
uartMaster = do
  uartBase
  setKind "UartMaster"
  setIdent "UartMaster"
  return ()

uartSlave :: (IsPort p) => p ()
uartSlave = do
  i2cBase
  setKind "UartSlave"
  setIdent "UartSlave"
  return ()

i2cBase :: (IsPort p) => p ()
i2cBase = do
  powerSource
  powerSink
  setType [
    "highVoltage" <:= FloatC unknown,
    "lowVoltage" <:= FloatC unknown,
    "limitHighVoltage" <:= FloatC unknown,
    "limitLowVoltage" <:= FloatC unknown
    ]
  setType [
    "frequency" <:= FloatC unknown
    ]
  return ()

i2cMaster :: (IsPort p) => p ()
i2cMaster = do
  i2cBase
  setKind "I2cMaster"
  setIdent "I2cMaster"
  return ()

i2cSlave :: (IsPort p) => p ()
i2cSlave = do
  i2cBase
  setKind "I2cSlave"
  setIdent "I2cSlave"
  setType [
    "id" <:= IntC unknown
    ]
  return ()

-- Links

i2cLink :: Int -> Link ()
i2cLink numSlaves = do
  setIdent "I2cLink"
  setSignature "I2cLink"

  power <- addPort "power" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0))
      ]
    return()

  master <- addPort "master" $ do
    i2cMaster
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0)),  -- TODO: make this nonzero?
      "controlName" <:= StringV "i2c"
      ]
    return()

  slaves <- forM @[] [1..numSlaves] $ \ slaveId ->
    addPort ("slave" ++ (show slaveId)) $ do
      i2cSlave
      setType [
        "current" <:= (range (FloatV 0) (FloatV 0))
        ]
      return()

  constrain $ port master connected
  constrain $ Any (map (\ slave -> port slave connected) slaves)

  constrain $ port master (typeVal "voltage.min") :== Lit (FloatV 0)
  constrain $ port master (typeVal "voltage.max") :== port power (typeVal "voltage.max")
  constrain $ rSubset (port master (typeVal "voltage")) (port master (typeVal "limitVoltage"))

  constrain $ port master (typeVal "current") :== port power (typeVal "current")
  constrain $ rSubset (port master (typeVal "current")) (port master (typeVal "limitCurrent"))

  constrain $ port master (typeVal "highVoltage") :== port power (typeVal "voltage.min")

  forM slaves $ \ slave -> do
    constrain $ port slave (typeVal "voltage") :== port master (typeVal "voltage")
    constrain $ rSubset (port slave (typeVal "voltage")) (port slave (typeVal "limitVoltage"))

    constrain $ rSubset (port slave (typeVal "current")) (port slave (typeVal "limitCurrent"))

    constrain $ port slave (typeVal "highVoltage") :== port power (typeVal "voltage.min")
    constrain $ port master (typeVal "lowVoltage") :<= port slave (typeVal "limitLowVoltage")
    constrain $ port slave (typeVal "lowVoltage") :<= port master (typeVal "limitLowVoltage")
    constrain $ port master (typeVal "highVoltage") :>= port slave (typeVal "limitHighVoltage")
    constrain $ port slave (typeVal "highVoltage") :>= port master (typeVal "limitHighVoltage")

    constrain $ port master (typeVal "controlUid") :== port slave (typeVal "controlUid")

    -- TODO unique i2c id
    -- TODO frequency

  return ()
