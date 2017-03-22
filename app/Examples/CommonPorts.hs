
module Examples.CommonPorts where

import EDG

-- Basic port types
swPort :: (IsPort p) => p ()
swPort = do
  setIdent "SW-Interface"
  setKind "SW"
  setType [
      "data" <:= unknown
    , "apiDir" <:= StringC $ oneOf ["producer","consumer"]
    ]
  return ()

-- Power output
powerOut :: (IsPort p) => p ()
powerOut = do
  setIdent "PowerOut"

  -- They also have kinds, which are **very important**
  --
  -- Ports can only connect to other ports with the **same kind**, the system
  -- won't even try tentative connections with ports that don't match.
  --
  -- This is a very good reason to have port definitions that can be used on
  -- both links and modules, since it lets you be significantly more precise
  -- with their kinds, and limit the number of possible connections.
  setKind "VOUT"
  setType [
      "voltage" <:= FloatC $ unknown
    , "current" <:= FloatC $ unknown
    ]
  return ()

-- | Since these are all normal haskell objects, it's easy enough to use
--   functions to create a port, module, or link.
--
--   The 'IsPort' constraint lets you use this function as a ModulePort or
--   a LinkPort.
--
--   In general you should use port names that are relative to the module side
--   of the port. e.g. use "powerOut" for "power flowing out of the module" and
--   not "powerIn" for "power flowing into the link".

-- Statically parameterized power output
fixedPowerOut :: forall p. (IsPort p)
              => String -- Identifier prefix
              -> Float -- Voltage
              -> Float -- Voltage Tolerance (symmetric +/-)
              -> Float -- Max Current Out
              -> p ()
fixedPowerOut identPrefix v vErr maxI = do
  powerOut

  -- Ports also have mandatory human-readable identifiers.
  appendIdent identPrefix

  -- Also they have mandatory types.
  setType [
      "voltage" <:= FloatC $ v +/- vErr
    ]

  -- You can even set constraints over them.
  -- TODO: example to be moved elsewhere, we already statically set the voltage
  -- constrain $ typeVal "voltage" :>= (Lit $ FloatV 0.0)

  -- These will be expressed in the SMT solver, so prefer constraints in the
  -- types themselves when possible.
  -- For instance these constraints:
  --
  -- > constrain $ typeVal "current" :>= (Lit $ FloatV 0.0 )
  -- > constrain $ typeVal "current" :<= (Lit $ FloatV 20.0)
  --
  -- Could instead be rendered as:
  -- TODO: example to be moved elsewhere
  --
  setType ["current" <:= FloatC $ between 0 maxI]
  --
  -- Which should have the added benefit of informing you that the value is
  -- unusable in the pre-processing state, where there is some debug info.
  -- It's not perfect, but a lot better than a flat "Unsatisfiable" from the
  -- SAT solver.

  -- You can also end a definition for a port, link, or module with a
  -- 'return ()` instead of an endDef.
  -- They are identical, and 'return ()' is probably better practice.
  return ()

-- Power input
powerIn :: (IsPort p) => p ()
powerIn = do
  setIdent "PowerIn"
  setKind "VIN"
  setType [
      "voltage" <:= FloatC $ unknown
    , "current" <:= FloatC $ unknown
    ]
  constrain $ Not connected :=> (typeVal "current" :== (Lit $ FloatV 0))
  return ()

-- The hardware side of a GPIO port
gpioHW :: (IsPort p) => p ()
gpioHW = do
  setIdent "GPIO HW Port"
  setKind "GPIOHW"
  setType [
      -- 'unknown' is used for elements where we don't know
      -- anything about the value at all.
      -- It's the constraint that every value is a member of.
      "current" <:= FloatC $ unknown -- Amps
      -- I'm pretty sure we want to split voltage/properties for
      -- multistate pins like this by state.
    , "voltage" <:= FloatC $ unknown -- Volts
    , "direction" <:= StringC $ oneOf ["I","O","IO"]
      -- Using unknown like this, means you don't know the
      -- the type of data either. In general this is good for
      -- records whose fields you don't know.
    , "data" <:= unknown
    ]

  return ()

-- The software side of a GPIO port
gpioSW :: (IsPort p) => p ()
gpioSW = do
  swPort
  setIdent "GPIO"
  setKind "GPIOSW"
  setType [
      "bandwidth" <:= FloatC $ unknown -- Hz
    , "direction" <:= StringC $ oneOf ["I","O","IO"]
    , "data" <:= unknown
    ]
  return ()

-- This is an interesting one, we're making the GPIO driver here
-- implicit, so that a single generic linktype is capable of capturing
-- all gpio connections, the assumption is that during reification
-- we'll replace this with the correct driver, in the meantime this
-- reduces our part load.
gpioLink :: Link ()
gpioLink = do
  setIdent "gpio link"
  setSignature "gpio link"
  -- the resource the link needs
  res <- addPort "resource" $ do
    return ()
  -- the sw interface port
  sw  <- addPort "software" $ do
    return ()
  -- the hw interface port
  hw <- addPort "hardware" $ do
    return ()
  -- ensure the software direction types are correct.

  -- ensure that all the properties between all the ports match up

  -- ensure that the correct port connection requirements exist
  return ()

powerLink :: Int  -- number of sinks
    -> Link ()
powerLink numSinks = do
  setIdent "power link"
  setSignature "power link"

  source <- addPort "source" $ do
    powerOut
    return()

  sinks <- flip mapM [1..numSinks] $ \ id ->
    addPort ("sink" ++ (show id)) $ do
      powerIn
      return()

  -- Ensure at lease one sink is connected
  constrain $ port source connected :== Any(map (\ sink -> port sink connected) sinks)

  flip mapM sinks $ \ sink ->
    constrain $ (port source $ typeVal "voltage")
            :== (port sink   $ typeVal "voltage")
  constrain $ (port source $ typeVal "current")
          :== Sum(map (\ sink -> port sink $ typeVal "current") sinks)

  return ()
swLink :: Link ()
swLink = undefined
