module NewEncoding.CommsPorts where

import Data.List (tails)
import Control.Monad

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts

usbHost :: (IsPort p) => p ()
usbHost = do
  powerSource
  setKind "UsbHost"
  setIdent "UsbHost"
  return ()


usbDevice :: (IsPort p) => p ()
usbDevice = do
  powerSink
  setKind "usbDevice"
  setIdent "usbDevice"
  return ()


spiBase :: (IsPort p) => p ()
spiBase = do
  digitalBidirBase
  controllable
  setType [
    "frequency" <:= range (FloatC unknown) (FloatC unknown)
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
    "baud" <:= range (FloatC unknown) (FloatC unknown)
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
  uartBase
  setKind "UartSlave"
  setIdent "UartSlave"
  return ()

i2cBase :: (IsPort p) => p ()
i2cBase = do
  powerSource
  powerSink
  controllable
  setType [
    "1VoltageLevel" <:= FloatC unknown,
    "0VoltageLevel" <:= FloatC unknown,
    "limit1VoltageLevel" <:= FloatC unknown,
    "limit0VoltageLevel" <:= FloatC unknown
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


-- NOTE :: Try not to use lists just to decompose them. "!!" is a partial
--         function and generally to be avoided. If you have a fixed length
--         use, try to use tuples.
--
-- shamelessly lifted from https://wiki.haskell.org/99_questions/Solutions/26
-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _  = return []
-- combinations n xs = do y:xs' <- tails xs
--                        ys <- combinations (n-1) xs'
--                        return (y:ys)
