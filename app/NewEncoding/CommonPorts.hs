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
    "linkDomain" <:= StringV "HW",
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
    "linkDomain" <:= StringV "HW",
    -- The range of currents this source is capable of providing.
    "limitCurrent" <:= (range (FloatC unknown) (FloatC unknown))
    ]

  -- Make sure that the currents we see at runtime are a subset of the
  -- currents this link is capable of handling.
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")

  return ()


-- DC-driven motor ports
motorSink :: (IsPort p) => p ()
motorSink = do
  powerBase
  controllable
  setKind "MotorSink"
  setIdent "MotorSink"

  setType [
    "linkDomain" <:= StringV "HW,FW",
    -- The range of acceptable voltages this link is capable of handling
    "limitVoltage" <:= range (FloatC unknown) (FloatC unknown),
    -- Minimum required driving (high) voltage
    "limitDriveVoltage" <:= FloatC unknown
    ]

  -- Make sure that the voltages we see at runtime are a subset of the
  -- voltages this link is capable of handling.
  constrain $ rSubset (typeVal "voltage") (typeVal "limitVoltage")

  return ()

motorSource :: (IsPort p) => p ()
motorSource = do
  powerBase
  controllable
  setKind "MotorSource"
  setIdent "MotorSource"
  setType [
    "linkDomain" <:= StringV "HW,FW",
    -- The range of currents this source is capable of providing.
    "limitCurrent" <:= range (FloatC unknown) (FloatC unknown),
    -- Minimum driving (high) voltage
    "driveVoltage" <:= FloatC unknown
    ]

  -- Make sure that the currents we see at runtime are a subset of the
  -- currents this link is capable of handling.
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")

  return ()


analogControlBase :: (IsPort p) => p ()
analogControlBase = do
  controllable
  setType [
    "linkDomain" <:= StringV "HW,FW",
    -- The direction the api is flowing over the link in sw lang
    "apiDir" <:= StringC $ oneOf ["producer", "consumer"],
    -- Bit resolution of the signal
    "requiredBits" <:= FloatC unknown,
    -- Effective bit resolution limit of the driver / DAC / sink / ADC
    "limitBits" <:= FloatC unknown
    ]
  constrain $ (typeVal "requiredBits") :<= (typeVal "limitBits")
  return ()

analogSink :: (IsPort p) => p ()
analogSink = do
  powerBase
  analogControlBase
  setKind "AnalogSink"
  setIdent "AnalogSink"

  setType [
    -- The range of acceptable voltages this link is capable of handling
    "limitVoltage" <:= range (FloatC unknown) (FloatC unknown),
    -- Linear range of the input
    "limitScale" <:= range (FloatC unknown) (FloatC unknown)
    ]

  -- Make sure that the voltages we see at runtime are a subset of the
  -- voltages this link is capable of handling.
  constrain $ rSubset (typeVal "voltage") (typeVal "limitVoltage")

  return ()

analogSource :: (IsPort p) => p ()
analogSource = do
  powerBase
  analogControlBase
  setKind "AnalogSource"
  setIdent "AnalogSource"
  setType [
    -- The range of currents this source is capable of providing.
    "limitCurrent" <:= range (FloatC unknown) (FloatC unknown),
    -- Useful signal voltage range
    "scale" <:= range (FloatC unknown) (FloatC unknown)
    ]

  -- Make sure that the currents we see at runtime are a subset of the
  -- currents this link is capable of handling.
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")

  return ()

controllable :: (IsPort p) => p ()
controllable = do
  setType [
    -- The UID of the device (typically a microcontroller) controlling this link
    "controlUid" <:= UID,
    -- The used assigned name for the element being controlled.
    "controlName" <:= StringC unknown
    ]
  return ()

digitalControlBase :: (IsPort p) => p ()
digitalControlBase = do
  controllable
  setType [
    "linkDomain" <:= StringV "HW,FW",
    -- The direction the api is flowing over the link in sw lang
    "apiDir" <:= StringC $ oneOf ["producer", "consumer"],
    -- Is this an On/Off type API for this digital line or a PWM signal?
    "apiType" <:= StringC $ oneOf ["onOff", "pwm"]
    ]
  return ()

digitalSink :: (IsPort p) => p ()
digitalSink = do
  powerBase
  digitalControlBase
  setKind "DigitalSink"
  setIdent "DigitalSink"
  setType [
    -- The range of acceptable voltages this link is capable of handling
    "limitVoltage" <:= (range (FloatC unknown) (FloatC unknown)),

    -- The voltage above which a signal will be interpretered as a 1
    "limit1VoltageLevel" <:= FloatC unknown,
    -- The voltage below which a signal will be interpretered as a 0
    "limit0VoltageLevel" <:= FloatC unknown
    ]
  constrain $ rSubset (typeVal "voltage") (typeVal "limitVoltage")
  return ()

digitalSource :: (IsPort p) => p ()
digitalSource = do
  powerBase
  digitalControlBase
  setKind "DigitalSource"
  setIdent "DigitalSource"
  setType [
    -- The range of currents this source is capable of providing.
    "limitCurrent" <:= (range (FloatC unknown) (FloatC unknown)),

    -- The minimum guaranteed output voltage of a logic 1
    "1VoltageLevel" <:= FloatC unknown,
    -- The minimum guaranteed output voltage of a logic 0
    "0VoltageLevel" <:= FloatC unknown
    ]
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")
  return ()

digitalBidirBase :: (IsPort p) => p ()
digitalBidirBase = do
  powerBase
  setType [
    -- The range of acceptable voltages this link is capable of handling
    "limitVoltage" <:= (range (FloatC unknown) (FloatC unknown)),
    -- The range of currents this source is capable of providing.
    "limitCurrent" <:= (range (FloatC unknown) (FloatC unknown)),


    -- The voltage the source uses for a 1
      "1VoltageLevel" <:= FloatC unknown
    -- The voltage the source uses for a 0
    , "0VoltageLevel" <:= FloatC unknown
    -- The voltage above which a signal will be interpretered as a 1
    , "limit1VoltageLevel" <:= FloatC unknown
    -- The voltage below which a signal will be interpretered as a 0
    , "limit0VoltageLevel" <:= FloatC unknown
    ]
  constrain $ rSubset (typeVal "voltage") (typeVal "limitVoltage")
  constrain $ rSubset (typeVal "current") (typeVal "limitCurrent")
  return ()

digitalBidir :: (IsPort p) => p ()
digitalBidir = do
  digitalBidirBase
  digitalControlBase
  setKind "DigitalBidir"
  setIdent "DigitalBidir"
  setType [
    "linkDomain" <:= StringV "HW,FW",
    "digitalDir" <:= StringC $ oneOf ["source", "sink"]
    ]
  return ()

apiBase :: (IsPort p) => p ()
apiBase = do
  controllable
  setType [
    "linkDomain" <:= StringV "FW",
    "apiType" <:= StringC unknown,
    "apiData" <:= Record unknown,
    "deviceData" <:= Record unknown
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
