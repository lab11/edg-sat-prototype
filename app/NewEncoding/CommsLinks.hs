
module NewEncoding.CommsLinks where

import Data.List (tails)
import Control.Monad

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommsPorts

uartLink :: Link ()
uartLink = do
  setIdent "UartLink"
  setSignature "UartLink"

  master <- addPort "master" $ do
    uartMaster
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0))
      ]
    return()

  slave <- addPort "slave" $ do
    uartSlave
    setType [
      "current" <:= (range (FloatV 0) (FloatV 0))
      ]
    return()

  ensureConnected [master, slave]

  setFieldsEq False [master, slave] ["controlUid", "controlName"]

  constrain $ voltageLevelCheck master slave
  constrain $ voltageLevelCheck slave  master

  constrain $ rNotDisjoint (port master (typeVal "baud")) (port slave (typeVal "baud"))

  return ()

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

  ensureConnected [vin, vout]

  setFieldsEq False [vin, vout] ["current", "voltage", "limitVoltage"]

  return ()
