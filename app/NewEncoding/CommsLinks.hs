
module NewEncoding.CommsLinks where

import Data.List (tails)
import Control.Monad

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommsPorts

-- Electrical Links
usbLink :: Link ()
usbLink = do
  setIdent "UsbLink"
  setSignature "UsbLink"

  host <- addPort "host" usbHost
  device <- addPort "device" usbDevice

  ensureConnected [host, device]
  setFieldsEq False [host, device] ["voltage", "current"]

  return ()

uartLink :: Link ()
uartLink = do
  setIdent "UartLink"
  setSignature "UartLink"

  master <- addPort "master" $ do
    uartMaster
    return()

  slave <- addPort "slave" $ do
    uartSlave
    return()

  ensureConnected [master, slave]

  setFieldsEq False [master, slave] ["controlUid", "controlName"]

  constrain $ voltageLevelCheck master slave
  constrain $ voltageLevelCheck slave  master

  constrain $ rNotDisjoint (port master (typeVal "baud")) (port slave (typeVal "baud"))

  return ()

spiLink :: Int -> Link ()
spiLink numSlaves = do
  setIdent ("SpiLink" ++ (show numSlaves))
  setSignature "SpiLink"

  master <- addPort "master" $ do
    spiMaster
    setType [
      "controlName" <:= StringV "spi"
      ]
    return()

  slaves <- forM @[] [1..numSlaves] $ \ slaveId ->
    addPort ("slave" ++ (show slaveId)) spiSlave

  ensureConnected [master]
  constrain $ Any (map (\ slave -> port slave connected) slaves)

  setFieldsEq False (master : slaves) ["voltage", "controlUid"]

  forM slaves $ \ slave -> do
    constrain $ voltageLevelCheck slave  master
    constrain $ voltageLevelCheck master slave

    constrain $ rNotDisjoint (port master (typeVal "frequency")) (port slave (typeVal "frequency"))

    -- constrain $ port master (typeVal "controlUid") :== port slave (typeVal "controlUid")

  return ()

i2cLink :: Int -> Link ()
i2cLink numSlaves = do
  setIdent ("I2cLink" ++ (show numSlaves))
  setSignature "I2cLink"

  power <- addPort "power" $ do
    i2cPowerSink
    setType [
      "current" <:= (range (FloatV 0) (FloatV 2e-3)),
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36)) -- TODO: dummy constraint
      ]
    return()

  master <- addPort "master" $ do
    i2cMaster
    setType [
      "controlName" <:= StringV "i2c"
      ]
    return()

  slaves <- forM @[] [1..numSlaves] $ \ slaveId ->
    addPort ("slave" ++ (show slaveId)) $ do
      i2cSlave
      return()

  ensureConnected [power, master]
  constrain $ Any (map (\ slave -> port slave connected) slaves)

  constrain $ port master (typeVal "voltage.min")   :== Lit (FloatV 0)
  constrain $ port master (typeVal "voltage.max")   :== port power (typeVal "voltage.max")
  constrain $ port master (typeVal "1VoltageLevel") :== port power (typeVal "voltage.min")

  setFieldsEq False (master : slaves) ["voltage", "controlUid"]

  forM slaves $ \ slave -> do
    constrain $ port slave (typeVal "1VoltageLevel") :== port power (typeVal "voltage.min")
    constrain $ voltageLevelCheck slave  master
    constrain $ voltageLevelCheck master slave

    constrain $ rNotDisjoint (port master (typeVal "frequency")) (port slave (typeVal "frequency"))

  forM (allPairs slaves) $ \ (slave1,slave2) -> do
    constrain $ port slave1 (typeVal "id") :/= port slave2 (typeVal "id")

  return ()

-- NOTE :: Kept here since it's an integral part of an I2C power link.
i2cPower :: Module ()
i2cPower = do  -- aka the two pull up resistors
  setIdent "i2cPower"
  setSignature "i2cPower"
  setType []

  vin <- addPort "vin" $ do
    powerSink
    setType [
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()

  vout <- addPort "vout" $ do
    i2cPowerSink
    setType [
      "limitVoltage" <:= (range (FloatV 0) (FloatV 36))
      ]
    return ()

  ensureConnected [vin, vout]

  setFieldsEq False [vin, vout] ["current", "voltage", "limitVoltage"]

  return ()
