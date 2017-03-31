module NewEncoding.CommsPorts where

import Data.List (tails)
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
  controllable
  setType [
    "highVoltage" <:= FloatC unknown,
    "lowVoltage" <:= FloatC unknown,
    "limitHighVoltage" <:= FloatC unknown,
    "limitLowVoltage" <:= FloatC unknown
    ]
  setType [
    "frequency" <:= range (FloatC unknown) (FloatC unknown)
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

i2cPowerSink :: (IsPort p) => p ()
i2cPowerSink = do
  powerSink
  setKind "i2cPowerSource"
  setIdent "i2cPowerSource"
  return ()

i2cPower :: Module ()
i2cPower = do  -- aka the two pull up resistors
  setIdent "i2cPower"
  setSignature "i2cPower"
  setType []

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0)),  -- TODO i2c signal power draw
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()

  vout <- addPort "vout" $ do
    i2cPowerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()

  constrain $ port vin connected
  constrain $ port vout connected

  constrain $ port vin (typeVal "current") :== port vout (typeVal "current")
  constrain $ port vin (typeVal "voltage") :== port vout (typeVal "voltage")
  constrain $ port vin (typeVal "limitVoltage") :== port vout (typeVal "limitVoltage")

  return ()

-- shamelessly lifted from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

i2cLink :: Int -> Link ()
i2cLink numSlaves = do
  setIdent ("I2cLink" ++ (show numSlaves))
  setSignature "I2cLink"

  power <- addPort "power" $ do
    i2cPowerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36)) -- TODO: dummy constraint
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

  constrain $ port power connected
  constrain $ port master connected
  constrain $ Any (map (\ slave -> port slave connected) slaves)

  constrain $ port master (typeVal "voltage.min") :== Lit (FloatV 0)
  constrain $ port master (typeVal "voltage.max") :== port power (typeVal "voltage.max")
  constrain $ rSubset (port master (typeVal "voltage")) (port master (typeVal "limitVoltage"))

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

    constrain $ rNotDisjoint (port slave (typeVal "frequency")) (port slave (typeVal "frequency"))

    constrain $ port master (typeVal "controlUid") :== port slave (typeVal "controlUid")

  forM (combinations 2 slaves) $ \ combSlaves -> do
    constrain $ port (combSlaves !! 0) (typeVal "id") :/= port (combSlaves !! 1) (typeVal "id")

    -- TODO frequency

  return ()
