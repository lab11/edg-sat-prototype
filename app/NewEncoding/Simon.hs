module NewEncoding.Simon where

import EDG
import NewEncoding.CommonPorts
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.ChipModules

import Control.Monad

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
  modules = [
    ("button", 4, button),
    ("led", 4, led),
    ("mcu", 1, mcu)
    ],
  links = [
    ("apiLink", 8, apiLink),
    ("powerLink", 2, powerLink 4),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 1, digitalBidirSourceLink),
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

  leds <- forM @[] [1..0] $ \ id -> addPort ("led" ++ (show id)) $ do
    apiConsumer
    setType [
      "controlName" <:= StringV ("led" ++ (show id)),
      "apiType" <:= StringV "led",
      "apiData" <:= Record [
        "bandwidth" <:= FloatV 500
        ]
      ]
    return ()

  buttons <- forM @[] [1..0] $ \ id -> addPort ("button" ++ (show id)) $ do
    apiConsumer
    setType [
      "controlName" <:= StringV ("button" ++ (show id)),
      "apiType" <:= StringV "button",
      "apiData" <:= Record [
        "bandwidth" <:= FloatV 500
        ]
      ]
    return ()

  tsense <- addPort "tsense" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "tsense",
      "apiType" <:= StringV "temperatureSensor",
      "apiData" <:= Record unknown
      ]
    return ()

  let allPorts = (buttons ++ leds ++ [tsense])
  forM allPorts (\ portId -> constrain $ port portId connected)
  forM allPorts (\ portId -> constrain $ port portId (typeVal "controlUid") :== (typeVal "controlUid"))

  return ()


-- Some general notes about this:
--
-- - We don't keep track of which MCU each piece of software is running
--   on, there's nothing stopping the system from plopping down two MCUs and
--   not realizing there's no way to split the SW across them.
--   Fixing this is left as an exercise for the reader.
run :: EDGSettings -> IO ()
run = makeSynthFunc testLibrary [("Seed",seed)]
