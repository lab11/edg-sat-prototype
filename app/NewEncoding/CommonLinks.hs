
module NewEncoding.CommonLinks where

import EDG

import Control.Monad

import NewEncoding.Util
import NewEncoding.CommonPorts

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
      -- Make sure that the actual voltage range is a subset of the known
      -- limit.
      return()

  ensureConnected [source]

  -- Make sure that there exists at least one connected sink
  constrain $ Any (map (\ sink -> port sink connected) sinks)

  -- Make sure the current range supplied by the source is the same as the
  -- current range that the sinks can all draw from.
  constrain $ port source (typeVal "current.min") :== Sum (map (\ sink -> port sink (typeVal "current.min")) sinks)
  constrain $ port source (typeVal "current.max") :== Sum (map (\ sink -> port sink (typeVal "current.max")) sinks)

  -- Make sure all the voltages are equal
  setFieldsEq False (source : sinks) ["voltage"]

  return ()

-- | Ensure that we're creating and constraining the threshold voltages in
--   digital bidirectional links
digitalBidirVoltageConstraints :: ()
  => [PortName] -- bidir
  -> [PortName] -- source
  -> [PortName] -- sink
  -> Link ()
digitalBidirVoltageConstraints bidirs sources sinks = do

  -- Add the fields to the type
  setType [
      "highVoltage" <:= FloatC unknown
    , "lowVoltage"  <:= FloatC unknown
    , "limitHighVoltage" <:= FloatC unknown
    , "limitLowVoltage"  <:= FloatC unknown
    ]

  -- Connect the source fields to the module type
  setFieldsEq True (bidirs ++ sources) [
      "lowVoltage"
    , "highVoltage"
    ]

  -- Connect the sink fields to the module type
  setFieldsEq True (bidirs ++ sinks) [
      "limitLowVoltage"
    , "limitHighVoltage"
    ]

  -- Ensure that the read levels and write levels for this link are
  -- sensible.
  constrain $ typeVal "lowVoltage"  :<= typeVal "limitLowVoltage"
  constrain $ typeVal "highVoltage" :>= typeVal "limitHighVoltage"

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

  ensureConnected [{-source ,-} sink]

  setFieldsEq False [source,sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  digitalBidirVoltageConstraints [] [source] [sink]

  -- constrain $ (
  --     ((port source (typeVal "apiDir") :== Lit (StringV "producer"))
  --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
  --     ) :|| (
  --       (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
  --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
  --     ))

  return ()


digitalBidirLink :: Link ()
digitalBidirLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  bidir <- addPort "bidir" $ do
    digitalBidir
    setType [
      "apiDir" <:= StringV "producer"
      ]
    return()

  source <- addPort "source" $ do
    digitalSource
    setType [
      "apiDir" <:= StringV "consumer"
      ]
    return()

  sink <- addPort "sink" $ do
    digitalSink
    setType [
      "apiDir" <:= StringV "consumer"
      ]
    return()

  -- ensureConnected [bidir]

  -- Exactly one of the two ports is connected
  constrain $ port sink connected :<+> port source connected
  -- If the bidir is a source, then the sink is connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source"))
    :=> port sink connected
  -- And vice versa
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink"))
    :=> port source connected

  -- Shuffle other important fields around as needed.
  setFieldsEq False [bidir,source{- ,sink-}] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  digitalBidirVoltageConstraints [bidir] [source] [sink]

  -- hard constrained since only GPIOs are bidir (for now)
  -- constrain $ (
  --     ((port bidir (typeVal "apiDir") :== Lit (StringV "producer"))
  --       :&& (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
  --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
  --     ) :|| (
  --       (port bidir (typeVal "apiDir") :== Lit (StringV "consumer"))
  --       :&& (port source (typeVal "apiDir") :== Lit (StringV "producer"))
  --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
  --     ))

  return ()

digitalBidirSinkLink :: Link ()
digitalBidirSinkLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"


  bidir <- addPort "bidir" $ do
    digitalBidir
    setType [
      "apiDir" <:= StringV "producer",
      "digitalDir" <:= StringV "source"
      ]
    return()

  sink <- addPort "sink" $ do
    digitalSink
    setType [
      "apiDir" <:= StringV "consumer"
      ]
    return()

  ensureConnected [{-bidir,-} sink ]

  setFieldsEq False [bidir,sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  digitalBidirVoltageConstraints [bidir] [] [sink]

  return ()

digitalBidirSourceLink :: Link ()
digitalBidirSourceLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  bidir <- addPort "bidir" $ do
    digitalBidir
    setType [
      "apiDir" <:= StringV "producer",
      "digitalDir" <:= StringV "sink"
      ]
    return()

  source <- addPort "source" $ do
    digitalSource
    setType [
      "apiDir" <:= StringV "consumer"
      ]
    return()

  ensureConnected [bidir,source]

  setFieldsEq False [bidir,source] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  digitalBidirVoltageConstraints [bidir] [source] []

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

  ensureConnected [producer,consumer]

  setFieldsEq True [producer,consumer] [
      "controlUid"
    , "controlName"
    , "apiType"
    , "apiData"
    ]

  return ()
