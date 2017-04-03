module NewEncoding.Simon where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules

import Control.Monad

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
  modules = [
    ("i2cPower", 1, i2cPower),
    ("button", 1, button),
    ("led", 1, led),
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("lcd5v", 1, serialLcd16x2_5v),
    ("sdcard", 1, sdcard),
    -- ("pcf8575", 1, pcf8575),
    ("mcu", 1, mcu)
    ],
  links = [
    ("apiLink", 6, apiLink),
    ("powerLink", 2, powerLink 8),
    ("usbLink", 1, usbLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 1, digitalBidirSourceLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2),
    ("digitalLink", 0, digitalLink)
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
      "voltage" <:= (range (FloatV 4.5) (FloatV 5.5)),
      "limitCurrent" <:= (range (FloatV 0) (FloatV 0.5))
      ]
    return ()

  leds <- makePorts 1 "led" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "led",
      "apiData" <:= Record [
        "bandwidth" <:= FloatV 500
        ]
      ]
    return ()

  buttons <- makePorts 1 "button" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "button",
      "apiData" <:= Record [
        "bandwidth" <:= FloatV 500
        ]
      ]
    return ()

  tsenses <- makePorts 1 "tsense" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "temperatureSensor"
      ]
    return ()

  lcds <- makePorts 1 "lcd" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "characterLcd"
      ]
    return ()

  storages <- makePorts 1 "sdCard" $ \ name -> do
    apiConsumer
    setType [
      "controlName" <:= StringV name,
      "apiType" <:= StringV "nvmemory"
      ]
    return ()

  let allPorts = buttons ++ leds ++ tsenses ++ lcds ++ storages

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
run = makeSynthFunc testLibrary [("Seed",seed)]
