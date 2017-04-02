
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


  constrain $ port source (typeVal "current.min") :== Sum (map (\ sink -> port sink (typeVal "current.min")) sinks)
  constrain $ port source (typeVal "current.max") :== Sum (map (\ sink -> port sink (typeVal "current.max")) sinks)

  setFieldsEq False (source : sinks) ["voltage"]

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

  constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))

  constrain $ port source (typeVal "0LevelVoltage") :== port sink (typeVal "0LevelVoltage")
  constrain $ port source (typeVal "1LevelVoltage") :== port sink (typeVal "1LevelVoltage")
  constrain $ port sink (typeVal "0LevelVoltage") :<= port sink (typeVal "limit0LevelVoltage")
  constrain $ port sink (typeVal "1LevelVoltage") :>= port sink (typeVal "limit1LevelVoltage")

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

  constrain $ port bidir connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source")) :=> port sink connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink")) :=> port source connected

  constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port bidir (typeVal "current") :== port source (typeVal "current")
  constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")

  constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))
  constrain $ rSubset (port bidir (typeVal "voltage")) (port bidir (typeVal "limitVoltage"))
  constrain $ rSubset (port bidir (typeVal "current")) (port bidir (typeVal "limitCurrent"))

  constrain $ port bidir (typeVal "0LevelVoltage") :== port source (typeVal "0LevelVoltage")
  constrain $ port bidir (typeVal "0LevelVoltage") :== port sink (typeVal "0LevelVoltage")
  constrain $ port bidir (typeVal "1LevelVoltage") :== port source (typeVal "1LevelVoltage")
  constrain $ port bidir (typeVal "1LevelVoltage") :== port sink (typeVal "1LevelVoltage")

  constrain $ port sink (typeVal "0LevelVoltage") :<= port sink (typeVal "limit0LevelVoltage")
  constrain $ port bidir (typeVal "0LevelVoltage") :<= port bidir (typeVal "limit0LevelVoltage")
  constrain $ port sink (typeVal "1LevelVoltage") :>= port sink (typeVal "limit1LevelVoltage")
  constrain $ port bidir (typeVal "1LevelVoltage") :>= port bidir (typeVal "limit1LevelVoltage")

  constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")
  constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")
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

  constrain $ port bidir connected
  constrain $ port sink connected

  constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")

  constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  constrain $ rSubset (port bidir (typeVal "current")) (port bidir (typeVal "limitCurrent"))

  -- NOTE :: Not actually true, we the levels of the GPIO and the levels for
  --         the connected device DO NOT HAVE TO BE THE SAME.
  -- constrain $ port bidir (typeVal "0LevelVoltage") :== port sink (typeVal "0LevelVoltage")
  -- constrain $ port bidir (typeVal "1LevelVoltage") :== port sink (typeVal "1LevelVoltage")

  constrain $ port bidir (typeVal "0LevelVoltage") :<= port sink (typeVal "limit0LevelVoltage")
  constrain $ port bidir (typeVal "1LevelVoltage") :>= port sink (typeVal "limit1LevelVoltage")

  constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")

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

  constrain $ port bidir connected
  constrain $ port source connected

  constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  constrain $ port bidir (typeVal "current") :== port source (typeVal "current")

  constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))
  constrain $ rSubset (port bidir (typeVal "voltage")) (port bidir (typeVal "limitVoltage"))

  -- NOTE :: Not actually true, we the levels of the GPIO and the levels for
  --         the conect device DO NOT HAVE TO BE THE SAME.
  -- constrain $ port bidir (typeVal "0LevelVoltage") :== port source (typeVal "0LevelVoltage")
  -- constrain $ port bidir (typeVal "1LevelVoltage") :== port source (typeVal "1LevelVoltage")

  constrain $ port source (typeVal "0LevelVoltage") :<= port bidir (typeVal "limit0LevelVoltage")
  constrain $ port source (typeVal "1LevelVoltage") :>= port bidir (typeVal "limit1LevelVoltage")

  constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")


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

-- -- Electrical Links
-- powerLink :: Int -> Link ()
-- powerLink numSinks = do
--   setIdent "PowerLink"
--   setSignature "PowerLink"
--
--   source <- addPort "source" $ do
--     powerSource
--     return()
--
--   sinks <- forM @[] [1..numSinks] $ \ sinkId ->
--     addPort ("sink" ++ (show sinkId)) $ do
--       powerSink
--       -- Make sure that the actual voltage range is a subset of the known
--       -- limit.
--       return()
--
--   ensureConnected [source]
--
--   -- Make sure that there exists at least one connected sink
--   constrain $ Any (map (\ sink -> port sink connected) sinks)
--
--   -- Make sure the current range supplied by the source is the same as the
--   -- current range that the sinks can all draw from.
--   constrain $ port source (typeVal "current.min") :== Sum (map (\ sink -> port sink (typeVal "current.min")) sinks)
--   constrain $ port source (typeVal "current.max") :== Sum (map (\ sink -> port sink (typeVal "current.max")) sinks)
--
--   -- Make sure all the voltages are equal
--   setFieldsEq False (source : sinks) ["voltage"]
--
--   return ()
--
-- -- | Ensure that we're creating and constraining the threshold voltages in
-- --   digital bidirectional links
-- digitalBidirVoltageConstraints :: ()
--   => [PortName] -- bidir
--   -> [PortName] -- source
--   -> [PortName] -- sink
--   -> Link ()
-- digitalBidirVoltageConstraints bidirs sources sinks = do
--
--   -- Add the fields to the type
--   setType [
--       "1LevelVoltage" <:= FloatC unknown
--     , "0LevelVoltage"  <:= FloatC unknown
--     , "limit1LevelVoltage" <:= FloatC unknown
--     , "limit0LevelVoltage"  <:= FloatC unknown
--     ]
--
--   -- Connect the source fields to the module type
--   setFieldsEq True (bidirs ++ sources) [
--       "0LevelVoltage"
--     , "1LevelVoltage"
--     ]
--
--   -- Connect the sink fields to the module type
--   setFieldsEq True (bidirs ++ sinks) [
--       "limit0LevelVoltage"
--     , "limit1LevelVoltage"
--     ]
--
--   -- Ensure that the read levels and write levels for this link are
--   -- sensible.
--   constrain $ typeVal "0LevelVoltage"  :<= typeVal "limit0LevelVoltage"
--   constrain $ typeVal "1LevelVoltage" :>= typeVal "limit1LevelVoltage"
--
--   return ()
--
-- digitalLink :: Link ()
-- digitalLink = do
--   setIdent "DigitalLink"
--   setSignature "DigitalLink"
--
--   source <- addPort "source" $ do
--     digitalSource
--     return()
--
--   sink <- addPort "sink" $ do
--     digitalSink
--     return()
--
--   ensureConnected [{-source ,-} sink]
--
--   setFieldsEq False [source,sink] [
--       "voltage"
--     , "current"
--     , "controlUid"
--     , "controlName"
--     , "apiType"
--     ]
--
--   digitalBidirVoltageConstraints [] [source] [sink]
--
--   -- constrain $ (
--   --     ((port source (typeVal "apiDir") :== Lit (StringV "producer"))
--   --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
--   --     ) :|| (
--   --       (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
--   --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
--   --     ))
--
--   return ()
--
--
-- digitalBidirLink :: Link ()
-- digitalBidirLink = do
--   setIdent "DigitalLink"
--   setSignature "DigitalLink"
--
--   bidir <- addPort "bidir" $ do
--     digitalBidir
--     setType [
--       "apiDir" <:= StringV "producer"
--       ]
--     return()
--
--   source <- addPort "source" $ do
--     digitalSource
--     setType [
--       "apiDir" <:= StringV "consumer"
--       ]
--     return()
--
--   sink <- addPort "sink" $ do
--     digitalSink
--     setType [
--       "apiDir" <:= StringV "consumer"
--       ]
--     return()
--
--   -- ensureConnected [bidir]
--
--   -- Exactly one of the two ports is connected
--   constrain $ port sink connected :<+> port source connected
--   -- If the bidir is a source, then the sink is connected
--   constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source"))
--     :=> port sink connected
--   -- And vice versa
--   constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink"))
--     :=> port source connected
--
--   -- Shuffle other important fields around as needed.
--   setFieldsEq False [bidir,source{- ,sink-}] [
--       "voltage"
--     , "current"
--     , "controlUid"
--     , "controlName"
--     , "apiType"
--     ]
--
--   digitalBidirVoltageConstraints [bidir] [source] [sink]
--
--   -- hard constrained since only GPIOs are bidir (for now)
--   -- constrain $ (
--   --     ((port bidir (typeVal "apiDir") :== Lit (StringV "producer"))
--   --       :&& (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
--   --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
--   --     ) :|| (
--   --       (port bidir (typeVal "apiDir") :== Lit (StringV "consumer"))
--   --       :&& (port source (typeVal "apiDir") :== Lit (StringV "producer"))
--   --       :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
--   --     ))
--
--   return ()
--
-- digitalBidirSinkLink :: Link ()
-- digitalBidirSinkLink = do
--   setIdent "DigitalLink"
--   setSignature "DigitalLink"
--
--
--   bidir <- addPort "bidir" $ do
--     digitalBidir
--     setType [
--       "apiDir" <:= StringV "producer",
--       "digitalDir" <:= StringV "source"
--       ]
--     return()
--
--   sink <- addPort "sink" $ do
--     digitalSink
--     setType [
--       "apiDir" <:= StringV "consumer"
--       ]
--     return()
--
--   ensureConnected [{-bidir,-} sink ]
--
--   setFieldsEq False [bidir,sink] [
--       "voltage"
--     , "current"
--     , "controlUid"
--     , "controlName"
--     , "apiType"
--     ]
--
--   digitalBidirVoltageConstraints [bidir] [] [sink]
--
--   return ()
--
-- digitalBidirSourceLink :: Link ()
-- digitalBidirSourceLink = do
--   setIdent "DigitalLink"
--   setSignature "DigitalLink"
--
--   bidir <- addPort "bidir" $ do
--     digitalBidir
--     setType [
--       "apiDir" <:= StringV "producer",
--       "digitalDir" <:= StringV "sink"
--       ]
--     return()
--
--   source <- addPort "source" $ do
--     digitalSource
--     setType [
--       "apiDir" <:= StringV "consumer"
--       ]
--     return()
--
--   ensureConnected [bidir,source]
--
--   setFieldsEq False [bidir,source] [
--       "voltage"
--     , "current"
--     , "controlUid"
--     , "controlName"
--     , "apiType"
--     ]
--
--   digitalBidirVoltageConstraints [bidir] [source] []
--
--   return ()
--
-- -- Seed Links
-- apiLink :: Link ()
-- apiLink = do
--   setIdent "ApiLink"
--   setSignature "ApiLink"
--
--   producer <- addPort "producer" $ do
--     apiProducer
--     return()
--
--   consumer <- addPort "consumer" $ do
--     apiConsumer
--     return()
--
--   ensureConnected [producer,consumer]
--
--   setFieldsEq True [producer,consumer] [
--       "controlUid"
--     , "controlName"
--     , "apiType"
--     , "apiData"
--     ]
--
--   return ()
