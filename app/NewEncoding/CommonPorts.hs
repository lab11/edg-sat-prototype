module NewEncoding.CommonPorts where

import EDG
import Control.Monad

powerBase :: (IsPort p) => p ()
powerBase = do
  setType [
    -- The concrete range of voltages expected to be seen at runtime.
    "voltage" <:= (range (FloatC unknown) (FloatC unknown)),
    -- The concrete set of currents expected to be seen at runtime.
    "current" <:= (range (FloatC unknown) (FloatC unknown))
    ]
  constrain $ Not connected :=> (typeVal "current.min") :== Lit (FloatV 0)
  constrain $ Not connected :=> (typeVal "current.max") :== Lit (FloatV 0)
  return ()

powerSink :: (IsPort p) => p ()
powerSink = do
  powerBase
  setKind "PowerSink"
  setIdent "PowerSink"

  setType [
    -- The range of acceptable voltages this link is capable of handling
    "limitVoltage" <:= (range (FloatC unknown) (FloatC unknown))
    ]

  -- Make sure that the voltages we see at runtime are a subset of the
  -- voltages this link is capable of handling.
  constrain $ rSubset (typeVal "voltage") (typeVal "limitVoltage")

  return ()

powerSource :: (IsPort p) => p ()
powerSource = do
  powerBase
  setKind "PowerSource"
  setIdent "PowerSource"
  setType [
    -- The range of currents this source is capable of handling.
    "limitCurrent" <:= (range (FloatC unknown) (FloatC unknown))
    ]

  -- Make sure that the currents we see at runtime are a subset of the
  -- currents this link is capable of handling.
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")

  return ()

controllable :: (IsPort p) => p ()
controllable = do
  setType [
    -- The UID of the element being controlled.
    "controlUid" <:= UID,
    -- The used assigned name for the element being controlled.
    "controlName" <:= StringC unknown
    ]
  return ()

digitalControlBase :: (IsPort p) => p ()
digitalControlBase = do
  controllable
  setType [
    -- The direction the api is flowing over the link in sw lang
    "apiDir" <:= StringC $ oneOf ["producer", "consumer"],
    -- Is this an On/Off type API for this digital line or a PWM signal?
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
    -- The lower limit for the high voltage threshold
    "limit1VoltageLevel" <:= FloatC unknown,
    -- The upper limir for the low voltage threshold
    "limit0VoltageLevel" <:= FloatC unknown
    ]
  return ()

digitalSource :: (IsPort p) => p ()
digitalSource = do
  powerSource
  digitalControlBase
  setKind "DigitalSource"
  setIdent "DigitalSource"
  setType [
    -- The voltage the source uses for a 1
    "1VoltageLevel" <:= FloatC unknown,
    -- The voltage the source uses for a 0
    "0VoltageLevel" <:= FloatC unknown
    ]
  return ()

digitalBidirBase :: (IsPort p) => p ()
digitalBidirBase = do
  powerSource
  powerSink
  setType [
    -- The voltage the source uses for a 1
      "1VoltageLevel" <:= FloatC unknown
    -- The voltage the source uses for a 0
    , "0VoltageLevel" <:= FloatC unknown
    -- The lower limit for the high voltage threshold
    , "limit1VoltageLevel" <:= FloatC unknown
    -- The upper limir for the low voltage threshold
    , "limit0VoltageLevel" <:= FloatC unknown
    ]
  -- Ensure that the read levels and write levels for this port are
  -- sensible.
  -- constrain $ typeVal "0VoltageLevel"  :<= typeVal "limit0VoltageLevel"
  -- constrain $ typeVal "1VoltageLevel" :>= typeVal "limit1VoltageLevel"
  return ()

digitalBidir :: (IsPort p) => p ()
digitalBidir = do
  digitalBidirBase
  digitalControlBase
  setKind "DigitalBidir"
  setIdent "DigitalBidir"
  setType [
    "digitalDir" <:= StringC $ oneOf ["source", "sink"]
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
