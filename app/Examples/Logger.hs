
module Examples.Logger where

import EDG
import Examples.CommonPorts
import Examples.CommonModules

-- SPI defines
spiMaster :: (IsPort p) => p ()
spiMaster = do
  setIdent "spiMaster"
  setKind "spiMaster"
  setType [
      "frequency" <:= FloatC $ unknown
    ]
  return ()

spiSlave :: (IsPort p) => p ()
spiSlave = do
  setIdent "spiSlave"
  setKind "spiSlave"
  setType [
      "frequency" <:= FloatC $ unknown
    , "mode" <:= IntC $ oneOf [0, 1, 2, 3]  -- TODO: better way of defining CPOL/CPHA
    ]
  return ()

sdcard :: Module()
sdcard = do
  setIdent "sdcard"
  setSignature "sdcard"
  setType []
  vin <- addPort "vin" $ do
    powerIn
    setType [
        "voltage" <:= FloatC $ between 2.7 3.6
      , "current" <:= FloatV 0.200  -- TODO: current draw ranges
      ]
    return ()
  spiSlave <- addPort "spiSlave" $ do
    spiSlave
    setType [
        "frequency" <:= FloatC $ between 0 25000000
      , "mode" <:= IntV 0
      ]
    return ()
  spiCs <- addPort "spiCs" $ do
    -- gpioHw  -- TODO: add this back in
    setType [
        "direction" <:= StringV "I"
      , "data" <:= Record [
              "signal" <:= StringV "SPI CS"
            , "id" <:= UID
          ]
      ]
    return ()
  -- TODO: optional 4-pin SDIO port, mutually exclusive (connectivity) with SPI mode
  constrain $ port vin connected
  constrain $ port spiSlave connected :&& port spiCs connected

  return ()

battery :: Module()
battery = do
  setIdent "battery"
  setSignature "battery"
  setType [
      "capacity" <:= FloatC $ unknown  -- capacity in Ah
    , "voltage" <:= FloatC $ unknown
    , "runtime" <:= FloatC $ unknown
    , "dischargeC" <:= FloatC $ unknown
    ]
  vout <- addPort "vout" $ do
    powerOut
    setType [
        "voltage" <:= FloatC $ between 2.7 3.6
      , "current" <:= FloatV 0.200  -- TODO: current draw ranges
      ]
    return ()
  return ()

  constrain $ port vout (typeVal "voltage") :== typeVal "voltage"
  constrain $ port vout (typeVal "current") :<= typeVal "dischargeC" :* typeVal "capacity"
