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

import Control.Monad

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
