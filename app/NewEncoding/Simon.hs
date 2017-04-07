module NewEncoding.Simon where

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
    ("button", 4, button),
    ("led", 4, led),

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
