module NewEncoding.Datalogger where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules
import NewEncoding.SwAdapters
import NewEncoding.RedundantModules
import NewEncoding.Design

import Control.Monad

minLibrary :: EDGLibrary
minLibrary = EDGLibrary{
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- More devices
    ("tmp102", 1, tmp102),
    ("sdcard", 1, sdcard),

    ("fat32", 1, fat32),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 3, apiLink),

    ("powerLink", 1, powerLink 4),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),

    ("spiLink", 1, spiLink 2),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

minOlLibrary :: EDGLibrary
minOlLibrary = EDGLibrary{
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- More devices
    ("tmp102", 1, tmp102),

    ("openLog", 1, openLog),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 3, apiLink),

    ("powerLink", 1, powerLink 4),
    ("usbLink", 1, usbLink),

    ("i2cLink", 1, i2cLink 2),
    ("uartLink", 1, uartLink)
    ]
  }


medLibrary :: EDGLibrary
medLibrary = EDGLibrary{
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- Basic devices
    ("button", 2, button),
    ("led", 2, led),

    -- More devices
    ("tmp102", 1, tmp102),
    ("sdcard", 1, sdcard),

    ("fat32", 1, fat32),
    ("openLog", 1, openLog),

    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("domeButton", 2, domeButton),
    ("qre1113Analog", 1, qre1113Analog),

    ("powerControlFan", 1, pwmControlFan),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3),


    -- Interfaces
    ("pcf8575", 1, pcf8575),
    ("litButton", 2, litButton),

    ("l7805", 1, l7805)

    ],
  links = [
    ("apiLink", 6, apiLink),

    ("powerLink", 1, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),

    ("spiLink", 1, spiLink 2),
    ("i2cLink", 1, i2cLink 2),

    ("digitalBidirSourceLink", 3, digitalBidirSourceLink),

    ("motorLink", 1, motorLink),
    ("analogLink", 1, analogLink)
    ]
  }


seed :: Module ()
seed = do
  setIdent "Control Logic"
  setSignature "controlLogic"

  setType [
    "controlUid" <:= UID
    ]

  usbHost <- addPort "usbHost" $ do
    usbHost
    setType[
      "voltage" <:= range (FloatV 4.5) (FloatV 5.5),
      "limitCurrent" <:= range (FloatV 0) (FloatV 0.5)
      ]
    return ()

  sensor <- addPort "sensor" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "sensor",
      "apiType" <:= StringV "temperatureSensor"
      ]
    constrain $ typeVal "apiData.tempRange.min" :<= Lit (FloatV 0)
    constrain $ typeVal "apiData.tempRange.max" :>= Lit (FloatV 75)
    constrain $ typeVal "apiData.tempResolution" :<= Lit (FloatV 0.5)
    return ()

  storage <- addPort "storage" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "storage",
      "apiType" <:= StringV "fat32"
      ]
    constrain $ typeVal "apiData.size" :>= Lit (IntV 1073741824)  -- 1 GiB
    return ()

  let allPorts = [sensor, storage]

  ensureConnected allPorts
  setFieldsEq True allPorts ["controlUid"]

  return ()


-- Some general notes about this:
--
-- - We don't keep track of which MCU each piece of software is running
--   on, there's nothing stopping the system from plopping down two MCUs and
--   not realizing there's no way to split the SW across them.
--   Fixing this is left as an exercise for the reader.
run :: EDGSettings -> IO ()
run = makeSynthFunc fullLibrary [("Seed",seed)]

minRun :: EDGSettings -> IO ()
minRun = makeSynthFunc minLibrary [("Seed",seed)]

minOlRun :: EDGSettings -> IO ()
minOlRun = makeSynthFunc minOlLibrary [("Seed",seed)]

medRun :: EDGSettings -> IO ()
medRun = makeSynthFunc medLibrary [("Seed",seed)]