
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

  source <- addPort "source" digitalSource

  sink <- addPort "sink" digitalSink

  ensureConnected [source, sink]
  -- constrain $ port source connected
  -- constrain $ port sink connected

  setFieldsEq False [source, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- NOTE :: We do NOT copy the digital voltage levels of the source into the
  --         sink, the sink DOES NOT HAVE voltage levels. It has level limits,
  --         and we do the comparisons in the relevant link.
  constrain $ port source (typeVal "0VoltageLevel") :<= port sink (typeVal "limit0VoltageLevel")
  constrain $ port source (typeVal "1VoltageLevel") :>= port sink (typeVal "limit1VoltageLevel")

  -- NOTE :: Promoted into the ports themselves
  -- constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  -- constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))

  constrain $ matchedPairs
    (port source $ typeVal "apiDir",port sink $ typeVal "apiDir")
    (Lit $ StringV "producer", Lit $ StringV "consumer")
    --   ((port source (typeVal "apiDir") :== Lit (StringV "producer"))
    --     :&& (port sink (typeVal "apiDir") :== Lit (StringV "consumer"))
    --   ) :|| (
    --     (port source (typeVal "apiDir") :== Lit (StringV "consumer"))
    --     :&& (port sink (typeVal "apiDir") :== Lit (StringV "producer"))
    --   ))

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
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "source")) :== port sink connected
  constrain $ (port bidir (typeVal "digitalDir") :== Lit (StringV "sink"  )) :== port source connected

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

  constrain $ port sink connected :=>
    (   (port bidir  (typeVal "0VoltageLevel") :<= port sink  (typeVal "limit0VoltageLevel"))
    :&& (port bidir  (typeVal "1VoltageLevel") :>= port sink  (typeVal "limit1VoltageLevel")))
  constrain $ port source connected :=>
    (   (port source (typeVal "0VoltageLevel") :<= port bidir (typeVal "limit0VoltageLevel"))
    :&& (port source (typeVal "1VoltageLevel") :>= port bidir (typeVal "limit1VoltageLevel")))

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

  setFieldsEq False [bidir, sink] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- constrain $ port bidir (typeVal "voltage") :== port sink (typeVal "voltage")
  -- constrain $ port bidir (typeVal "current") :== port sink (typeVal "current")
  -- constrain $ port bidir (typeVal "controlUid") :== port sink (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlName") :== port sink (typeVal "controlName")
  -- constrain $ port bidir (typeVal "apiType") :== port sink (typeVal "apiType")

  -- constrain $ rSubset (port sink (typeVal "voltage")) (port sink (typeVal "limitVoltage"))
  -- constrain $ rSubset (port bidir (typeVal "current")) (port bidir (typeVal "limitCurrent"))

  -- NOTE :: Not actually true, we the levels of the GPIO and the levels for
  --         the connected device DO NOT HAVE TO BE THE SAME.
  -- constrain $ port bidir (typeVal "0VoltageLevel") :== port sink (typeVal "0VoltageLevel")
  -- constrain $ port bidir (typeVal "1VoltageLevel") :== port sink (typeVal "1VoltageLevel")

  constrain $ port bidir (typeVal "0VoltageLevel") :<= port sink (typeVal "limit0VoltageLevel")
  constrain $ port bidir (typeVal "1VoltageLevel") :>= port sink (typeVal "limit1VoltageLevel")

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
  -- constrain $ port bidir connected
  -- constrain $ port sink connected

  setFieldsEq False [bidir, source] [
      "voltage"
    , "current"
    , "controlUid"
    , "controlName"
    , "apiType"
    ]

  -- constrain $ port bidir connected
  -- constrain $ port source connected

  -- constrain $ port bidir (typeVal "voltage") :== port source (typeVal "voltage")
  -- constrain $ port bidir (typeVal "current") :== port source (typeVal "current")
  -- constrain $ port bidir (typeVal "controlUid") :== port source (typeVal "controlUid")
  -- constrain $ port bidir (typeVal "controlName") :== port source (typeVal "controlName")
  -- constrain $ port bidir (typeVal "apiType") :== port source (typeVal "apiType")

  -- constrain $ rSubset (port source (typeVal "current")) (port source (typeVal "limitCurrent"))
  -- constrain $ rSubset (port bidir (typeVal "voltage")) (port bidir (typeVal "limitVoltage"))

  -- NOTE :: Not actually true, we the levels of the GPIO and the levels for
  --         the conect device DO NOT HAVE TO BE THE SAME.
  -- constrain $ port bidir (typeVal "0VoltageLevel") :== port source (typeVal "0VoltageLevel")
  -- constrain $ port bidir (typeVal "1VoltageLevel") :== port source (typeVal "1VoltageLevel")

  constrain $ port source (typeVal "0VoltageLevel") :<= port bidir (typeVal "limit0VoltageLevel")
  constrain $ port source (typeVal "1VoltageLevel") :>= port bidir (typeVal "limit1VoltageLevel")



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
--       "1VoltageLevel" <:= FloatC unknown
--     , "0VoltageLevel"  <:= FloatC unknown
--     , "limit1VoltageLevel" <:= FloatC unknown
--     , "limit0VoltageLevel"  <:= FloatC unknown
--     ]
--
--   -- Connect the source fields to the module type
--   setFieldsEq True (bidirs ++ sources) [
--       "0VoltageLevel"
--     , "1VoltageLevel"
--     ]
--
--   -- Connect the sink fields to the module type
--   setFieldsEq True (bidirs ++ sinks) [
--       "limit0VoltageLevel"
--     , "limit1VoltageLevel"
--     ]
--
--   -- Ensure that the read levels and write levels for this link are
--   -- sensible.
--   constrain $ typeVal "0VoltageLevel"  :<= typeVal "limit0VoltageLevel"
--   constrain $ typeVal "1VoltageLevel" :>= typeVal "limit1VoltageLevel"
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
