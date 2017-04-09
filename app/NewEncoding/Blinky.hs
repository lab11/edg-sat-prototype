module NewEncoding.Blinky where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules
import NewEncoding.Design
import NewEncoding.RedundantModules

import NewEncoding.SwAdapters
import Control.Monad



import NewEncoding.RedundantModules

import NewEncoding.SwAdapters


minLibrary :: EDGLibrary
minLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("button", 1, button),
    ("led", 1, led),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 2, apiLink),

    ("powerLink", 1, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 1, digitalBidirSourceLink)
    ]
  }

medLibrary :: EDGLibrary
medLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("button", 2, button),
    ("led", 2, led),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3),

    -- Base links
    ("i2cPower", 1, i2cPower),

    -- More devices
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("domeButton", 2, domeButton),
    ("sdcard", 1, sdcard),
    ("qre1113Analog", 1, qre1113Analog),

    ("powerControlFan", 1, pwmControlFan),

    -- Interfaces
    ("pcf8575", 1, pcf8575),
    ("litButton", 2, litButton),

    ("l7805", 1, l7805)

    ],
  links = [
    ("apiLink", 6, apiLink),

    ("powerLink", 1, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 3, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 3, digitalBidirSourceLink),

    ("motorLink", 1, motorLink),
    ("analogLink", 1, analogLink),

    ("i2cLink", 1, i2cLink 2)

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

  led <- addPort "led" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "led",
      "apiType" <:= StringV "led"
      ]
    -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
    return ()

  button <- addPort "button" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "button",
      "apiType" <:= StringV "button"
      ]
    -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
    return ()

  let allPorts = [led, button]

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

medRun :: EDGSettings -> IO ()
medRun = makeSynthFunc medLibrary [("Seed",seed)]