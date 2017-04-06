module NewEncoding.SimonTrinket where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules
import NewEncoding.RedundantModules
import NewEncoding.Design

import Control.Monad

minLibrary :: EDGLibrary
minLibrary = EDGLibrary{
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- Basic devices
    ("button", 4, button),
    ("led", 4, led),

    -- Interfaces
    ("pcf8575", 1, pcf8575),

    -- Microcontrollers
    ("trinket3v3", 1, arduinoTrinket3v3)
    ],
  links = [
    ("apiLink", 8, apiLink),

    ("powerLink", 2, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalBidirSinkLink", 4, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 4, digitalBidirSourceLink),

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

  leds <- makePorts 4 "led" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "led"
      ]
    -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
    return ()

  buttons <- makePorts 4 "button" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "button"
      ]
    -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
    return ()

  let allPorts = buttons ++ leds

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
run = makeSynthFunc minLibrary [("Seed",seed)]
