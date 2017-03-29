module NewEncoding.CommonPorts where

import EDG
import Control.Monad

powerBase :: (IsPort p) => p ()
powerBase = do
  setType [
    "voltage" <:= FloatC unknown,
    "current" <:= FloatC unknown
    ]
  return ()

powerSink :: (IsPort p) => p ()
powerSink = do
  powerBase
  setKind "PowerSink"
  setIdent "PowerSink"
  setType [
    "limitVoltage" <:= FloatC unknown
    ]
  return ()

powerSource :: (IsPort p) => p ()
powerSource = do
  powerBase
  setKind "PowerSource"
  setIdent "PowerSource"
  setType [
    "limitCurrent" <:= FloatC unknown
    ]
  return ()

controllable :: (IsPort p) => p ()
controllable = do
  setType [
    "controlUid" <:= StringC unknown,
    "controlName" <:= StringC unknown
    ]
  return ()

digitalControlBase :: (IsPort p) => p ()
digitalControlBase = do
  controllable
  setType [
    "apiDir" <:= StringC $ oneOf ["producer", "consumer"],
    "apiType" <:= StringC $ oneOf ["onOff", "pwm"]
    ]
  return ()

digitalSink :: (IsPort p) => p ()
digitalSink = do
  powerSink
  digitalControlBase
  setKind "DigitalSink"
  setIdent "DigitalSink"
  setType [
    -- "limitHighVoltage" <:= FloatC unknown,
    -- "limitLowVoltage" <:= FloatC unknown,
    ]
  return ()

digitalSource :: (IsPort p) => p ()
digitalSource = do
  powerSource
  digitalControlBase
  setKind "DigitalSource"
  setIdent "DigitalSource"
  setType [
    -- "highVoltage" <:= FloatC unknown,
    -- "lowVoltage" <:= FloatC unknown,
    ]
  return ()

digitalBidir :: (IsPort p) => p ()
digitalBidir = do
  powerSource
  powerSink
  digitalControlBase
  setKind "DigitalBidir"
  setIdent "DigitalBidir"
  setType [
    "digitalDir" <:= StringC $ oneOf ["source", "sink"]
    -- "highVoltage" <:= FloatC unknown,
    -- "lowVoltage" <:= FloatC unknown,
    ]
  return ()

apiBase :: (IsPort p) => p ()
apiBase = do
  controllable
  setType [
    "apiType" <:= StringC unknown,
    "apiData" <:= Record unknown
    ]
  return ()

apiProducer :: (IsPort p) => p ()
apiProducer = do
  apiBase
  setKind "ApiProducer"
  setIdent "ApiProducer"
  return ()

apiConsumer :: (IsPort p) => p ()
apiConsumer = do
  apiBase
  setKind "ApiConsumer"
  setIdent "ApiConsumer"
  return ()

-- Electrical Links
powerLink :: Int -> Link ()
powerLink numSinks = do
  setIdent "PowerLink"
  setSignature "PowerLink"

  source <- addPort "source" $ do
    powerSource
    return()

  sinks <- forM @[] [1..numSinks] $ \ sinkId ->
    addPort ("sink" ++ (show sinkId)) $ do
      powerSink
      return()

  constrain $ port source connected
  constrain $ Any (map (\ sink -> port sink connected) sinks)

  constrain $ port source (typeVal "current") :== Sum (map (\ sink -> port sink (typeVal "current")) sinks)
  constrain $ port source (typeVal "current") :<= port source (typeVal "limitCurrent")

  forM sinks $ \ sink -> do
    constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
    constrain $ port sink (typeVal "voltage") :== port sink (typeVal "limitVoltage")

  return ()

digitalLink :: Link ()
digitalLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  source <- addPort "source" $ do
    digitalSource
    return()

  sink <- addPort "sink" $ do
    digitalSink
    return()

  constrain $ port source connected
  constrain $ port sink connected

  constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port source (typeVal "current") :== port sink (typeVal "current")

  constrain $ port sink (typeVal "voltage") :<= port sink (typeVal "limitVoltage")
  constrain $ port source (typeVal "current") :<= port source (typeVal "limitCurrent")

  constrain $ port source (typeVal "controlUid") :== port sink (typeVal "controlUid")
  constrain $ port source (typeVal "controlName") :== port sink (typeVal "controlName")
  constrain $ port source (typeVal "apiType") :== port sink (typeVal "apiType")
  constrain $ (
      ((port source (typeVal "apiDir") :== Lit (StringV "producer"))
        :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
      ) :|| (
        (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
        :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
      ))

  return ()

digitalBidirLink :: Link ()
digitalBidirLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  bidir <- addPort "bidir" $ do
    digitalBidir
    return()

  source <- addPort "source" $ do
    digitalSource
    return()
  sink <- addPort "sink" $ do
    digitalSink
    return()

  constrain $ port bidir connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source")) :=> port sink connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink")) :=> port source connected

  constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port bidir (typeVal "current") :== port source (typeVal "current")
  constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")

  constrain $ port sink (typeVal "voltage") :<= port sink (typeVal "limitVoltage")
  constrain $ port source (typeVal "current") :<= port source (typeVal "limitCurrent")
  constrain $ port bidir (typeVal "voltage") :<= port bidir (typeVal "limitVoltage")
  constrain $ port bidir (typeVal "current") :<= port bidir (typeVal "limitCurrent")

  constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")
  constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")
  constrain $ (
      ((port bidir (typeVal "apiDir") :== Lit (StringV "producer"))
        :&& (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
        :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
      ) :|| (
        (port bidir (typeVal "apiDir") :== Lit (StringV "consumer"))
        :&& (port source (typeVal "apiDir") :== Lit (StringV "producer"))
        :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
      ))

  return ()

-- Seed Links
apiLink :: Link ()
apiLink = do
  setIdent "ApiLink"
  setSignature "ApiLink"

  producer <- addPort "producer" $ do
    apiProducer
    return()

  consumer <- addPort "consumer" $ do
    apiConsumer
    return()

  constrain $ port producer connected
  constrain $ port consumer connected

  constrain $ port producer (typeVal "apiType") :== port consumer (typeVal "apiType")
  constrain $ port producer (typeVal "apiData") :== port consumer (typeVal "apiData")
  constrain $ port producer (typeVal "controlUid") :== port consumer (typeVal "controlUid")
  constrain $ port producer (typeVal "controlName") :== port consumer (typeVal "controlName")
  return ()
