
module NewEncoding.CommonLinks where

import EDG

import Control.Monad

import NewEncoding.Util
import NewEncoding.CommonPorts

-- Electrical Links
powerLink :: Int -> Link ()
powerLink numSinks = do
  setIdent ("PowerLink" ++ (show numSinks))
  setSignature "PowerLink"

  -- NOTE :: You don't actually need the whole "do" wrapper, since you're
  --         doing nothing with it. Just insert the port directly.
  source <- addPort "source" powerSource

  sinks <- forM @[] [1..numSinks] $ \ sinkId ->
    addPort ("sink" ++ (show sinkId)) powerSink

  ensureConnected [source]
  constrain $ Any (map (\ sink -> port sink connected) sinks)

  -- NOTE :: We could technically replace the above Any constraint with a
  --         one that just ensures the first sink is used.
  --
  --         I tried this, and it took less CPU time but more Wall time.
  --         I have no fucking clue why. This is just weird.
  -- ensureConnected [source, head sinks]

  constrain $ port source (typeVal "current.min")
    :== Sum (map (\ sink -> port sink (typeVal "current.min")) sinks)
  constrain $ port source (typeVal "current.max")
    :== Sum (map (\ sink -> port sink (typeVal "current.max")) sinks)

  setFieldsEq False (source : sinks) ["voltage"]

  return ()

digitalLink :: Link ()
digitalLink = do
  setIdent "DigitalLink"
  setSignature "DigitalLink"

  source <- addPort "source" digitalSource
  sink <- addPort "sink" digitalSink

  ensureConnected [source, sink]

  setFieldsEq False [source, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  constrain $ voltageLevelCheck source sink

  constrain $ matchedPairs
    (port source $ typeVal "apiDir",port sink $ typeVal "apiDir")
    (Lit $ StringV "producer", Lit $ StringV "consumer")

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

  ensureConnected [bidir]

  -- NOTE :: Using an equal sign here doesn't change the semantics, but should
  --         allow the SMT solver to propagate information backwards from
  --         the source and sink, to the bidir, as well as the forward dir.
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source"))
    :== port sink connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink"  ))
    :== port source connected

  -- NOTE :: This xor is technically redundant, still likely useful for the
  --         SMT solver.
  constrain $ (port sink connected :<+> port source connected)

  setFieldsEq False [bidir, source, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- NOTE :: Predicating on the port should mean that the system doesn't bother
  --         picking values for things that don't matter.
  constrain $ port sink   connected :=> voltageLevelCheck bidir  sink
  constrain $ port source connected :=> voltageLevelCheck source bidir

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

  ensureConnected [bidir, sink]

  setFieldsEq False [bidir, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  constrain $ voltageLevelCheck bidir sink

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

  ensureConnected [bidir, source]

  setFieldsEq False [bidir, source] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  constrain $ voltageLevelCheck source bidir

  return ()

analogLink :: Link ()
analogLink = do
  setIdent "AnalogLink"
  setSignature "AnalogLink"

  source <- addPort "source" analogSource
  sink <- addPort "sink" analogSink

  ensureConnected [source, sink]

  setFieldsEq False [source, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "requiredBits"
    ]

  constrain $ rSubset (port source (typeVal "scale")) (port sink (typeVal "limitScale"))

  constrain $ matchedPairs
    (port source $ typeVal "apiDir",port sink $ typeVal "apiDir")
    (Lit $ StringV "producer", Lit $ StringV "consumer")

  return ()


motorLink :: Link ()
motorLink = do
  setIdent "MotorLink"
  setSignature "MotorLink"

  source <- addPort "source" motorSource
  sink <- addPort "sink" motorSink

  ensureConnected [source, sink]

  setFieldsEq False [source, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    ]

  constrain $ port source (typeVal "driveVoltage") :>= port sink (typeVal "limitDriveVoltage")

  return ()


-- Seed Links
apiLink :: Link ()
apiLink = do
  setIdent "ApiLink"
  setSignature "ApiLink"

  producer <- addPort "producer" apiProducer
  consumer <- addPort "consumer" apiConsumer

  ensureConnected [producer, consumer]

  setFieldsEq False [producer, consumer] [
      "controlUid"
    , "controlName"
    , "apiType"
    , "apiData"
    ]

  return ()
