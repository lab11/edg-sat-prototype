
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
  -- constrain $ port source connected

  -- Make sure that there exists at least one connected sink
  constrain $ Any (map (\ sink -> port sink connected) sinks)

  -- Make sure the current range supplied by the source is the same as the
  -- current range that the sinks can all draw from.
  constrain $ port source (typeVal "current.min") :== Sum (map (\ sink -> port sink (typeVal "current.min")) sinks)
  constrain $ port source (typeVal "current.max") :== Sum (map (\ sink -> port sink (typeVal "current.max")) sinks)

  -- Make sure all the voltages are equal
  setFieldsEq False (source : sinks) ["voltage"]

  -- forM sinks $ \ sink -> do
    -- NOTE :: Using field equality above
    -- constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
    -- NOTE :: Promoted to a constraint in the port itself.
    -- constrain $ rSubset (port sink $ typeVal "voltage") (port sink $ typeVal "limitVoltage")

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

  ensureConnected [source,sink]

  -- constrain $ port source connected
  -- constrain $ port sink connected

  setFieldsEq False [source,sink] [
      "voltage"
    , "current"
    , "lowVoltage"
    , "highVoltage"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- constrain $ port source (typeVal "voltage") :== port sink (typeVal "voltage")
  -- constrain $ port source (typeVal "current") :== port sink (typeVal "current")

  -- constrain $ port source (typeVal "lowVoltage") :== port sink (typeVal "lowVoltage")
  -- constrain $ port source (typeVal "highVoltage") :== port sink (typeVal "highVoltage")

  -- constrain $ port source (typeVal "controlUid") :== port sink (typeVal "controlUid")
  -- constrain $ port source (typeVal "controlName") :== port sink (typeVal "controlName")
  -- constrain $ port source (typeVal "apiType") :== port sink (typeVal "apiType")

  constrain $ port sink (typeVal "lowVoltage") :<= port sink (typeVal "limitLowVoltage")
  constrain $ port sink (typeVal "highVoltage") :>= port sink (typeVal "limitHighVoltage")

  constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))

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

  setType [
      "highVoltage" <:= FloatC unknown
    , "lowVoltage"  <:= FloatC unknown
    , "limitHighVoltage" <:= FloatC unknown
    , "limitLowVoltage"  <:= FloatC unknown
    ]

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

  setFieldsEq False [bidir,source,sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  setFieldsEq True [bidir,source] [
      "lowVoltage"
    , "highVoltage"
    ]

  setFieldsEq True [bidir,sink] [
      "limitLowVoltage"
    , "limitHighVoltage"
    ]

  -- constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  -- constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  -- constrain $ port bidir (typeVal "current") :== port source (typeVal "current")
  -- constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")
  --
  -- constrain $ port bidir (typeVal "lowVoltage") :== port source (typeVal "lowVoltage")
  -- constrain $ port bidir (typeVal "lowVoltage") :== port sink (typeVal "lowVoltage")
  -- constrain $ port bidir (typeVal "highVoltage") :== port source (typeVal "highVoltage")
  -- constrain $ port bidir (typeVal "highVoltage") :== port sink (typeVal "highVoltage")

  -- constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  -- constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  -- constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")
  -- constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")

  constrain $ port bidir connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source"))
    :=> port sink connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink"))
    :=> port source connected

  -- NOTE :: Promoted into the ports
  -- constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  -- constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))
  -- constrain $ rSubset (port bidir (typeVal "voltage")) (port bidir (typeVal "limitVoltage"))
  -- constrain $ rSubset (port bidir (typeVal "current")) (port bidir (typeVal "limitCurrent"))

  -- Ensure that the read levels and write levels for this link are
  -- sensible.
  constrain $ typeVal "lowVoltage"  :<= typeVal "limitLowVoltage"
  constrain $ typeVal "highVoltage" :>= typeVal "limitHighVoltage"

  -- NOTE :: Promoted into the link
  -- constrain $ port bidir (typeVal "lowVoltage") :<= port bidir (typeVal "limitLowVoltage")
  -- constrain $ port bidir (typeVal "highVoltage") :>= port bidir (typeVal "limitHighVoltage")

--  hard constrained since only GPIOs are bidir (for now)
--  constrain $ (
--      ((port bidir (typeVal "apiDir") :== Lit (StringV "producer"))
--        :&& (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
--        :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
--      ) :|| (
--        (port bidir (typeVal "apiDir") :== Lit (StringV "consumer"))
--        :&& (port source (typeVal "apiDir") :== Lit (StringV "producer"))
--        :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
--      ))

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

  -- constrain $ port bidir connected
  -- constrain $ port sink connected

  setFieldsEq [bidir,sink] [
      "voltage"
    , "current"
    , "lowVoltage"
    , "highVoltage"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  -- constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")

  -- constrain $ port bidir (typeVal "lowVoltage") :== port sink (typeVal "lowVoltage")
  -- constrain $ port bidir (typeVal "highVoltage") :== port sink (typeVal "highVoltage")

  -- constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  -- constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")

  -- NOTE :: Promoted into the port
  -- constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  -- constrain $ rSubset (port bidir (typeVal "current")) (port bidir (typeVal "limitCurrent"))

  constrain $ port sink (typeVal "lowVoltage") :<= port sink (typeVal "limitLowVoltage")
  constrain $ port sink (typeVal "highVoltage") :>= port sink (typeVal "limitHighVoltage")


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

  -- constrain $ port bidir connected
  -- constrain $ port source connected

  setFieldsEq [bidir,source] [
      "voltage"
    , "current"
    , "lowVoltage"
    , "highVoltage"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  -- constrain $ port bidir (typeVal "current") :== port source (typeVal "current")

  -- constrain $ port bidir (typeVal "lowVoltage") :== port source (typeVal "lowVoltage")
  -- constrain $ port bidir (typeVal "highVoltage") :== port source (typeVal "highVoltage")
  --
  -- constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  -- constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")


  constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))
  constrain $ rSubset (port bidir (typeVal "voltage")) (port bidir (typeVal "limitVoltage"))

  constrain $ port bidir (typeVal "lowVoltage") :<= port bidir (typeVal "limitLowVoltage")
  constrain $ port bidir (typeVal "highVoltage") :>= port bidir (typeVal "limitHighVoltage")



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

  setFieldsEq [producer,consumer] [
      "controlUid"
    , "controlName"
    , "apiType"
    , "apiData"
    ]

  -- constrain $ port producer (typeVal "controlUid") :== port consumer (typeVal "controlUid")
  -- constrain $ port producer (typeVal "controlName") :== port consumer (typeVal "controlName")
  --
  -- constrain $ port producer (typeVal "apiType") :== port consumer (typeVal "apiType")
  -- constrain $ port producer (typeVal "apiData") :== port consumer (typeVal "apiData")

  return ()
