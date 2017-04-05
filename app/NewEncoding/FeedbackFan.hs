module NewEncoding.FeedbackFan where

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

  sensor <- addPort "sensor" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "sensor",
      "apiType" <:= StringV "temperatureSensor"
      ]
    constrain $ typeVal "apiData.tempRange.min" :<= Lit (FloatV 10)  -- common human ranges
    constrain $ typeVal "apiData.tempRange.max" :>= Lit (FloatV 40)
    constrain $ typeVal "apiData.tempResolution" :<= Lit (FloatV 1)
    return ()

  display <- addPort "display" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "display",
      "apiType" <:= StringV "characterLcd"
      ]
    return ()

  fan <- addPort "fan" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "fan",
      "apiType" <:= StringV "controlledFan"
      ]
    return ()

  let allPorts = [sensor, display, fan]

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
