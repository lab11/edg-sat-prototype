module NewEncoding.Robot where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules
import NewEncoding.RedundantModules
import NewEncoding.SwAdapters
import NewEncoding.Design

import Control.Monad

minLibrary :: EDGLibrary
minLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("qre1113Analog", 2, qre1113Analog),

    -- Interfaces
    ("tb6612fng", 1, tb6612fng),
    ("l7805", 1, l7805),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 2, apiLink),

    ("powerLink", 2, powerLink 6),

    ("digitalBidirSinkLink", 6, digitalBidirSinkLink),
    ("motorLink", 2, motorLink),
    ("analogLink", 2, analogLink)

--    ("digitalBidirSinkLink", 3, digitalBidirSinkLink),
--    ("motorLink", 1, motorLink),
--    ("analogLink", 1, analogLink)
    ]
  }

medLibrary :: EDGLibrary
medLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("qre1113Analog", 2, qre1113Analog),
    ("button", 2, button),
    ("led", 2, led),

    -- Interfaces
    ("tb6612fng", 1, tb6612fng),
    ("l7805", 1, l7805),
    ("litButton", 2, litButton),

    -- Base links
    ("i2cPower", 1, i2cPower),

    -- More devices
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("domeButton", 2, domeButton),
    ("sdcard", 1, sdcard),
    ("qre1113Analog", 1, qre1113Analog),

    ("powerControlFan", 1, pwmControlFan),
    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 6, apiLink),

    ("powerLink", 3, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 6, digitalBidirSinkLink),
    ("motorLink", 2, motorLink),
    ("analogLink", 2, analogLink),

    ("digitalBidirSourceLink", 3, digitalBidirSourceLink),

    ("i2cLink", 1, i2cLink 2)

--    ("digitalBidirSinkLink", 3, digitalBidirSinkLink),
--    ("motorLink", 1, motorLink),
--    ("analogLink", 1, analogLink)
    ]
  }



seed :: Module ()
seed = do
  setIdent "Control Logic"
  setSignature "controlLogic"

  setType [
    "controlUid" <:= UID
    ]

  let makePorts i s m = forM @[] [1..i]
        (\ id -> let name = s ++ (show id) in addPort name $ m name)

  -- 2S lithium-ion / lithium-polymer battery
  power <- addPort "power" $ do
    powerSource
    setType[
      "voltage" <:= range (FloatV 7.4) (FloatV 8.4),
      "limitCurrent" <:= range (FloatV 0) (FloatV 3)
      ]
    return ()


  sensors <- makePorts 2 "sensor" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "reflectanceSensor"
      ]
    constrain $ typeVal "apiData.requiredBits" :== Lit(FloatV 8)
    return ()

  motors <- makePorts 2 "motor" $ \ name -> do
    motorSink
    setType [
      "limitVoltage" <:= range (FloatV 0) (FloatV 6),
      "limitDriveVoltage" <:= FloatV 3,
      "current" <:= range (FloatV 0) (FloatV 0.4),
      "controlName" <:= StringV name
      ]
    return ()

  let allPorts = sensors ++ motors

  ensureConnected allPorts
  setFieldsEq True allPorts ["controlUid"]

  setFieldsEq False sensors ["deviceData"]

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

medRun :: EDGSettings -> IO ()
medRun = makeSynthFunc medLibrary [("Seed",seed)]
