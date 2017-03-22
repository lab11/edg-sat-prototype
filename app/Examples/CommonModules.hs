
module Examples.CommonModules where

import EDG
import Examples.CommonPorts

-- | A button module
button :: Module ()
button = do
  setIdent "button"
  foo <- uniqueName "momSw"
  setSignature foo
  setType []
  vin <- addPort "vin" $ do
    powerIn
    setIdent "buttonVIN"
    setType [
        -- temporary static current draw, TODO: actual computed current draw
        "current" <:= FloatV 0.001
      ]
    return ()
  gpio <- addPort "gpio" $ do
    gpioHW
    setIdent "buttonGPIO"
    setType [
        "current" <:= FloatC unknown
      , "direction" <:= StringV "O"
      , "data" <:= switchData
      ]
    return ()
  -- Ports must both be connected for design to work
  constrain $ port vin connected
  -- constrain $ port gpio connected  -- TODO: when we have GPIO links working
  -- The ID of this part must end up in the port somehow
  constrain $ (port gpio $ typeVal "data.id") :== uid
  -- We can't draw more current through the thing than the
  -- GPIO pin can sink
  constrain $ (port vin $ typeVal "current")
    :== (port gpio $ typeVal "current")
  -- The voltage of our powersource and control pin must be the same.
  constrain $ (port vin $ typeVal "voltage")
    :== (port gpio $ typeVal "voltage")
  return ()

buttonDriver :: Module ()
buttonDriver = do
  setIdent "ButtonDriver"
  setSignature "signature"
  input <- addPort "gpioIn" $ do
    gpioSW
    setIdent "ButtonDriver"
    setType [
        "direction" <:= StringV $ "O"
      , "data" <:= switchData
      ]
    return ()
  output <- addPort "swAPIOut" $ do
    swPort
    setType [
        "apiDir" <:= StringV "producer"
      , "data" <:= switchData
      ]
    return ()
  constrain $ (port input  $ typeVal "data")
          :== (port output $ typeVal "data")
  --constrain $ port input connected :=> port output connected
  return ()

led :: Module ()
led = do
  setIdent "led"
  setSignature "led"
  setType []
  vin <- addPort "vin" $ do
    powerIn
    setIdent "ledVin"
    setType [
        "voltage" <:= FloatC $ greaterThan 2
      , "current" <:= FloatV 0.01  -- TODO: make this parameterizable / autocomputed?
      ]
    return ()
  gpio <- addPort "gpio" $ do
    gpioHW
    setIdent "ledGPIO"
    setType [
        "direction" <:= StringV "I"
      , "data" <:= Record [
              "signal" <:= StringV "LED"
            , "id" <:= UID
            , "name" <:= StringC unknown
          ]
      ]
    return ()
  constrain $ port vin connected
  constrain $ port gpio connected
  constrain $ (port gpio $ typeVal "data.id") :== uid
  constrain $ (port vin $ typeVal "voltage")
    :== (port gpio $ typeVal "voltage")
  return ()

ledDriver :: Module ()
ledDriver = do
  setIdent "LedDriver"
  setSignature "leddriver"
  input <- addPort "gpioIn" $ do
    gpioSW
    setIdent "LEDDriver"
    setType [
        "bandwidth" <:= FloatC $ greaterThan 500 -- Hz
      , "direction" <:= StringV $ "I"
      , "data" <:= ledData
      ]
    return ()
  output <- addPort "swAPIOut" $ do
    swPort
    setType [
        "apiDir" <:= StringV "producer"
      , "data" <:= ledData
      ]
    return ()
  constrain $ (port input  $ typeVal "data")
          :== (port output $ typeVal "data")
  constrain $ port input connected :== port output connected
  return ()
