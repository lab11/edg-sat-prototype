module NewEncoding.Simon where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules
import NewEncoding.SwAdapters
import NewEncoding.Design
import NewEncoding.RedundantModules


import Control.Monad

minLibrary :: EDGLibrary
minLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("button", 4, button),
    ("led", 4, led),
    ("litButton", 4, litButton),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 12, apiLink),

    ("powerLink", 1, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 4, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 4, digitalBidirSourceLink)
    ]
  }

medLibrary :: EDGLibrary
medLibrary = EDGLibrary{
  modules = [
    -- Basic devices
    ("button", 4, button),
    ("led", 4, led),
    ("litButton", 4, litButton),

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

    ("l7805", 1, l7805)

    ],
  links = [
    ("apiLink", 12, apiLink),

    ("powerLink", 1, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 4, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 4, digitalBidirSourceLink),

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

  pwr12v <- addPort "pwr12v" $ do
    powerSource
    setType[
      "voltage" <:= range (FloatV 11.8) (FloatV 12.2),  -- really good supply!
      "limitCurrent" <:= range (FloatV 0) (FloatV 3)  -- really beefy supply!
      ]
    return ()


  buttons <- makePorts 4 "button" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "litButton"
      ]
    -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
    return ()

  let allPorts = buttons

  ensureConnected allPorts
  setFieldsEq True allPorts ["controlUid"]
  setFieldsEq False buttons ["deviceData"]

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
