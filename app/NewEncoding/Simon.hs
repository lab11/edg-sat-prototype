module NewEncoding.Simon where

import EDG
import NewEncoding.CommonPorts
import NewEncoding.CommonModules

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
    modules = [
        ("button",1,button)
      , ("led",1,led)
      , ("mcu",1,mcu)
      ]
  , links   = [
        ("gpioControlLink", 4, gpioControlLink)
      , ("electricalLink", 2, electricalLink)
      , ("digitalLink", 2, digitalLink)
      ]
  }

seed :: Module ()
seed = do
  setIdent "Control Logic"
  setSignature "controlLogic"

  led1 <- addPort "led1" $ do
    ledControl
    setType [
        "name" <:= StringV "led1"
      , "dir" <:= StringV "consumer"
      , "bandwidth" <:= FloatV 500
      ]
    return ()

  constrain $ port led1 connected

  --button1 <- addPort "button1" $ do
  --  buttonControl
  --  setType [
  --      "name" <:= StringV "button1"
  --    , "dir" <:= StringV "consumer"
  --    , "bandwidth" <:= FloatV 500
  --    ]
  --  return ()

  --constrain $ port button1 connected

  return ()


-- Some general notes about this:
--
-- - We don't keep track of which MCU each piece of software is running
--   on, there's nothing stopping the system from plopping down two MCUs and
--   not realizing there's no way to split the SW across them.
--   Fixing this is left as an exercise for the reader.
run :: EDGSettings -> IO ()
run = makeSynthFunc testLibrary [("Seed",seed)]
