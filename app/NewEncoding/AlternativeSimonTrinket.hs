module NewEncoding.AlternativeSimonTrinket where

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
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- Basic devices
    ("domeButton", 4, domeButton),

    -- Interfaces
    ("pcf8575", 1, pcf8575),
    ("digitalAmplifier", 4, digitalAmplifier),

    -- Microcontrollers
    ("trinket3v3", 1, arduinoTrinket3v3)
    ],
  links = [
    ("apiLink", 4, apiLink),

    ("powerLink", 2, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 4, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 4, digitalBidirSourceLink),
    ("digitalLink", 4, digitalLink),

    ("i2cLink", 1, i2cLink 1)
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


  buttons <- makePorts 1 "button" $ \ name -> do
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
run = makeSynthFunc minLibrary [("Seed",seed)]
