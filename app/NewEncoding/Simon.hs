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
    ("i2cPower", 1, i2cPower),
    ("button", 0, button),
    ("led", 0, led),
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("lcd5v", 1, serialLcd16x2_5v),
    ("mcu", 1, mcu)
    ],
  links = [
    ("apiLink", 5, apiLink),
    ("powerLink", 2, powerLink 8),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 1, digitalBidirSourceLink),
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

  tsenses <- forM @[] [1..1] $ \ id -> addPort ("tsense" ++ (show id)) $ do
    apiConsumer
    setType [
      "controlName" <:= StringV ("tsense" ++ (show id)),
      "apiType" <:= StringV "temperatureSensor",
      "apiData" <:= Record unknown
      ]
    return ()

  lcd <- addPort "lcd" $ do
    apiConsumer
    setType [
      "controlName" <:= StringV "lcd",
      "apiType" <:= StringV "characterLcd",
      "apiData" <:= Record unknown
      ]
    return ()

  let allPorts = (buttons ++ leds ++ tsenses ++ [lcd])
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
