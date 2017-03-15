
module Examples.Simon where

import EDG

-- | This is a simple example of a microcontroller encoded so that we can
--   perform synthesis over it.
mcu :: Module ()
mcu = do
  -- Mandatory : A human readable identifier.
  -- This isn't used in synthesis anywhere, but makes reading the output of a
  -- run a damn sight easier.
  -- Optimize for convinience.
  setIdent "Arduino Pro Mini 3.3v 8MHz"

  -- Mandatory : A signature used to map parts with their implementation
  -- details in when doing reification and whatnot.
  setSignature "SFE DEV-11114"

  -- Mandatory : The type of the module, works pretty much how you'd expect.
  setType [
      -- There's only one operator for adding stuff to records now '<:='.
      -- It is used for everything, but you have to tag the types of your
      -- values correctly. Types like "IntV","FloatV", and "StringV", are
      -- used for concrete values, while types like "IntC", "FloatC", and
      -- "StringC" are used for constrained values.
      -- This kludge exists to save you having to annotate everything with
      -- explicit types, which gets old fast.
      "MHz" <:= FloatV 8.0

      -- As before, there's a pile of different types of constraints you can
      -- use to bound values, 'oneOf','noneOf','lessThan', and '+/-' being
      -- good examples.
      -- Putting multiple such constraints in a list is analogous to and-ing
      -- all of the subcosntraints together.
    , "exampleVoltage" <:= FloatC [5 +/- 0.5, lessThan 12]

      -- It's probably also good practice making values like this part of the
      -- type, though if you're doing something generic, don't set a single
      -- value like this, set a range of possible values.
      --
      -- Future calls to 'setType' and 'updateType' (literally the same
      -- function) can only add fields or *further constrain* the type.
    , "maxSourceCurrent" <:= FloatC $ greaterThan 0.0
    ]

  -- The 'addPort' function in a module or link allows you to add ports.
  -- The name given is the primary differentiator for ports.
  --
  -- **If you add two ports with the same name the system will see them as
  -- one port with the constraints of both!!**
  addPort "5vOut" $ powerOut "5v" 5.0 0.5  0.5

  -- In the context of a module, you can get a nice identifier for the port
  -- as a return value for the addPort action.
  -- This can be used in place of the name of the port, if you want your code
  -- to be a little more robust.
  p3v3 <- addPort "3v3Out" $ powerOut "3v3" 3.3 0.25 0.25

  -- We can even add multiple ports at one by chaining actions together using
  -- the standard combinators. For instance mapM maps over a list and uses some
  -- function to turn it into
  --
  -- flip f a b = f b a -- flips the arguments around
  gpios <- flip mapM ["gpio1","gpio2","gpio3","gpio4"] $ \ name ->
    -- In the function we're mapping with, we take the name of the port,
    -- constrain it further, and add it to the module.
    addPort name $ do
      -- We can then include our external global gpio port definition
      gpioRes

      -- Add useful information to the idents.
      appendIdent name

      -- And constrain the type signature a bit more.
      setType [
          "maxCurrent" <:= FloatC $ oneOf [0,3.3] -- Amps
        , "voltage" <:= FloatC $ 0.0 +/- 0.02 -- Volts
        , "bandwidth" <:= FloatV 1000 -- Hz
        ]

      return ()

  -- These resources represent the pins we have available to us on the
  -- MCU.
  pins <- mapM newResource $ map (\ n -> "pin" ++ (show n)) [1..8]

  -- We can make sure that each GPIO pin is capable of using any of the pins.
  --
  -- NOTE :: flip f a b = f b a
  -- NOTE :: mapM_ takes an '(a -> m b)'  and a '[a]' and gives you
  -- a 'm [b]' by applying your input fuction to each value in order
  -- and collecting the results together.
  flip mapM_ gpios (\ gpio -> do
    let constraintName :: String = gpio ++ "PinUse"
    -- Using 'constrainResources'
    -- Param 1 : Name
    -- Param 2 : Expression that, if true, means resources need to
    --           be allocated
    -- Param 3 : List of [<tag> :|= [list of acceptable resources]]
    --    If the expression is true, then each tag *must* be assigned
    --    one *and only one* resource from the list of acceptable resoruces.
    --    In this case, for each GPIO pin, we're checking whether it's
    --    connected, and if so, assigning one of the 8 pins.
    constrainResources constraintName (port gpio $ connected) ["Pin" :|= pins]
    )

  -- Now we can use further constrain the type signature of this module.
  -- Admittedly, in this case this is useless, but in cases where you're
  -- repeating a lot of work, it is probably useful to abstract the major
  -- constraints away, pull them into the type, and then constrain them
  -- for the specific element you're working with.
  updateType ["maxCurrent" <:= FloatV 5.0]

  -- We can also assemble constraints as if they were values here, it's
  -- super nice.
  --
  -- 'Sum' takes a list of numerical values and gives you their sum.
  -- It just unwraps into iterated ':+' under the hood, but whatever.
  --
  -- Note: ':' is the haskell cons operator so '[1,2,3] == (1:2:3:[])'
  constrain $ typeVal "maxSourceCurrent" :>= Sum (
    -- We can refer to ports by their names
    (port "5vOut" $ typeVal "maxCurrent") :
    -- Or using the identifiers we've gotten for them.
    (port p3v3 $ typeVal "maxCurrent") :
    -- and we can use normal Haskell syntax here
    (map (\ gpio -> port gpio $ typeVal "maxCurrent") gpios))

  -- For a number of reasons we need to end each definition of a module or
  -- port with either an 'endDef' or 'return ()' if you want to use it
  -- directly.
  --
  -- You can return some other value (like a reference to a value in the
  -- element) in order to do more advanced things, but we can talk about
  -- those later.
  --
  -- using 'return ()' is better practise but works slightly differently to
  -- how 'return' works in imperative languages. (It doesn't end the
  -- definition and close the block, it's only useful as the very last
  -- statement in a block)
  endDef

-- | Since these are all normal haskell objects, it's easy enough to use
--   functions to create a port, module, or link.
--
--   The 'IsPort' constraint lets you use this function as a ModulePort or
--   a LinkPort.
--
--   In general you should use port names that are relative to the module side
--   of the port. e.g. use "powerOut" for "power flowing out of the module" and
--   not "powerIn" for "power flowing into the link".
powerOut :: forall p. (IsPort p)
              => String -- Identifier prefix
              -> Float -- Voltage
              -> Float -- Error
              -> Float -- Max Current Out
              -> p ()
powerOut identPrefix v vErr maxI = do
  -- Ports also have mandatory human-readable identifiers.
  setIdent (identPrefix ++ "PowerOut")

  -- They also have kinds, which are **very important**
  --
  -- Ports can only connect to other ports with the **same kind**, the system
  -- won't even try tentative connections with ports that don't match.
  --
  -- This is a very good reason to have port definitions that can be used on
  -- both links and modules, since it lets you be significantly more precise
  -- with their kinds, and limit the number of possible connections.
  setKind "VOUT"

  -- Also they have mandatory types.
  setType [
      "voltage" <:= FloatC $ v +/- vErr
    , "maxCurrent" <:= FloatV maxI
    ]

  -- You can even set constraints over them.
  constrain $ typeVal "voltage" :>= (Lit $ FloatV 0.0)

  -- These will be expressed in the SMT solver, so prefer constraints in the
  -- types themselves when possible.
  -- For instance these constraints:
  --
  -- > constrain $ typeVal "maxCurrent" :>= (Lit $ FloatV 0.0 )
  -- > constrain $ typeVal "maxCurrent" :<= (Lit $ FloatV 20.0)
  --
  -- Could instead be rendered as:
  --
  setType ["maxCurrent" <:= FloatC $ between 0 20.0]
  --
  -- Which should have the added benefit of informing you that the value is
  -- unusable in the pre-processing state, where there is some debug info.
  -- It's not perfect, but a lot better than a flat "Unsatisfiable" from the
  -- SAT solver.

  -- You can also end a definition for a port, link, or module with a
  -- 'return ()` instead of an endDef.
  -- They are identical, and 'return ()' is probably better practice.
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
      "maxCurrent" <:= FloatC $ unknown -- Amps
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

-- The MCU resource that a GPIO port represents.
gpioRes :: (IsPort p) => p ()
gpioRes = do
  setIdent "gpio-resource"
  setKind "GPIORES"
  setType [
      -- Pin proerties
      "maxCurrent" <:= FloatC $ unknown -- Amps
    , "voltage" <:= FloatC $ unknown -- Volts
      -- Interface pro
    , "direction" <:= StringC $ oneOf ["I","O","IO"]
    , "bandwidth" <:= FloatC $ unknown -- Hz
    ]
  return ()

-- Power input
powerIn :: (IsPort p) => p ()
powerIn = do
  setIdent "PowerIn"
  setKind "VIN"
  setType [
      "voltage" <:= FloatC $ unknown
    , "maxCurrent" <:= FloatC $ unknown
    ]
  constrain $ typeVal "voltage" :>= (Lit $ FloatV 0.0)
  return ()

-- | A button module
button :: Module ()
button = do
  setIdent "button"
  setSignature "momentarySwitch"
  setType []
  vin <- addPort "vin" $ do
    powerIn
    setIdent "buttonVIN"
    setType [
        "maxCurrent" <:= FloatC [greaterThan 0.0, lessThan 0.001]
      ]
    return ()
  gpio <- addPort "gpio" $ do
    gpioHW
    setIdent "buttonGPIO"
    setType [
        "maxCurrent" <:= FloatC unknown
      , "direction" <:= StringV "O"
      , "data" <:= Record [
            "signal" <:= StringV "Momentary Switch"
          , "id" <:= UID
          ]
      ]
    return ()
  -- Ports must both be connected for design to work
  constrain $ port vin connected
  constrain $ port gpio connected
  -- The ID of this part must end up in the port somehow
  constrain $ (port gpio $ typeVal "data.id") :== uid
  -- We can't draw more current through the thing than the
  -- GPIO pin can sink
  constrain $ (port vin $ typeVal "maxCurrent")
    :== (port gpio $ typeVal "maxCurrent")
  -- The voltage of our powersource and control pin must be the same.
  constrain $ (port vin $ typeVal "voltage")
    :== (port gpio $ typeVal "voltage")
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
      , "maxCurrent" <:= FloatV 0.01
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
          ]
      ]
    return ()
  constrain $ port vin connected
  constrain $ port gpio connected
  constrain $ (port gpio $ typeVal "data.id") :== uid
  constrain $ (port vin $ typeVal "voltage")
    :== (port gpio $ typeVal "voltage")
  return ()

swPort :: (IsPort p) => p ()
swPort = do
  setIdent "SW-Interface"
  setKind "SW"
  setType [
      "data" <:= unknown
    , "apiDir" <:= StringC $ oneOf ["producer","consumer"]
    ]
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
      , "data" <:= Record [
            "signal" <:= StringV "LED"
          , "id" <:= UID
          ]
      ]
    return ()
  output <- addPort "swAPIOut" $ do
    swPort
    setIdent "LEDDriver"
    setType [
        "data" <:= unknown
      , "apiDir" <:= StringV "producer"
      ]
    return ()
  constrain $ (port input  $ typeVal "data")
          :== (port output $ typeVal "data")
  constrain $ port input connected :=> port output connected
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
      , "data" <:= Record [
            "signal" <:= StringV "Momentary Switch"
          , "id" <:= UID
          ]
      ]
    return ()
  output <- addPort "swAPIOut" $ do
    swPort
    setIdent "ButtonDriver"
    setType [
        "data" <:= unknown
      , "apiDir" <:= StringV "producer"
      ]
    return ()
  constrain $ (port input  $ typeVal "data")
          :== (port output $ typeVal "data")
  constrain $ port input connected :=> port output connected
  return ()

-- This is an interesting one, we're making the GPIO driver here
-- implicit, so that a single generic linktype is capable of capturing
-- all gpio connections, the assumption is that during reification
-- we'll replace this with the correct driver, in the meantime this
-- reduces our part load.
gpioLink :: Link ()
gpioLink = undefined

powerLink :: Link ()
powerLink = undefined

swLink :: Link ()
swLink = undefined

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
    modules = [
        ("button",3,button)
      , ("buttonDriver",3,buttonDriver)
      , ("led",3,led)
      , ("ledDriver",3,ledDriver)
      , ("mcu",1,mcu)
      ]
  , links   = [
      ]
  }

seed :: Module ()
seed = do
  setIdent "Control Logic"
  setSignature "controlLogic"
  setType []
  return ()


main :: IO ()
main = synthesize testLibrary "Seed" mcu