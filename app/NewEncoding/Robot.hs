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

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 2, apiLink),

    ("powerLink", 2, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 6, digitalBidirSinkLink),
    -- ("digitalBidirSourceLink", 3, digitalBidirSourceLink),
    ("motorLink", 2, motorLink),
    ("analogLink", 2, analogLink)
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

  usbHost <- addPort "usbHost" $ do
    usbHost
    setType[
      "voltage" <:= range (FloatV 4.5) (FloatV 5.5),
      "limitCurrent" <:= range (FloatV 0) (FloatV 0.5)
      ]
    return ()

  power <- addPort "power" $ do
    powerSource
    setType[
      "voltage" <:= range (FloatV 4.75) (FloatV 5.25),
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
run = makeSynthFunc minLibrary [("Seed",seed)]

