module NewEncoding.Datalogger where

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

  usbHost <- addPort "usbHost" $ do
    usbHost
    setType[
      "voltage" <:= range (FloatV 4.5) (FloatV 5.5),
      "limitCurrent" <:= range (FloatV 0) (FloatV 0.5)
      ]
    return ()

  sensor <- addPort "sensor" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "sensor",
      "apiType" <:= StringV "temperatureSensor",
      "apiData" <:= Record [
        "tempRange" <:= range (FloatC (lessThan 0)) (FloatC (greaterThan 75)),
        "tempResolution" <:= FloatC (lessThan 0.5)
        ]
      ]
    return ()

  storage <- addPort "storage" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "storage",
      "apiType" <:= StringV "nvmemory",
      "apiData" <:= Record [
        "size" <:= IntC (greaterThan 1073741824)  -- 1 GiB
        ]
      ]
    return ()

  let allPorts = [sensor, storage]

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
