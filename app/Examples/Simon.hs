
module Examples.Simon where

import EDG
import Examples.CommonPorts
import Examples.CommonModules

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
  addPort "5vOut" $ fixedPowerOut "5v" 5.0 0.5  0.5

  -- In the context of a module, you can get a nice identifier for the port
  -- as a return value for the addPort action.
  -- This can be used in place of the name of the port, if you want your code
  -- to be a little more robust.
  p3v3 <- addPort "3v3Out" $ fixedPowerOut "3v3" 3.3 0.25 0.25

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
          "current" <:= FloatC $ 0.0 +/- 0.02 -- Amps
        , "voltage" <:= FloatC $ 1.65 +/- 1.65 -- Volts
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
  updateType ["current" <:= FloatV 5.0]

  -- We can also assemble constraints as if they were values here, it's
  -- super nice.
  --
  -- 'Sum' takes a list of numerical values and gives you their sum.
  -- It just unwraps into iterated ':+' under the hood, but whatever.
  --
  -- Note: ':' is the haskell cons operator so '[1,2,3] == (1:2:3:[])'
  constrain $ typeVal "maxSourceCurrent" :>= Sum (
    -- We can refer to ports by their names
    (port "5vOut" $ typeVal "current") :
    -- Or using the identifiers we've gotten for them.
    (port p3v3 $ typeVal "current") :
    -- and we can use normal Haskell syntax here
    (map (\ gpio -> port gpio $ typeVal "current") gpios))

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

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
    modules = [
        ("button",1,button)
      , ("buttonDriver",1,buttonDriver)
      , ("led",1,led)
      , ("ledDriver",1,ledDriver)
      , ("mcu",1,mcu)
      ]
  , links   = [
        ("pwerLink",2,powerLink 4)
      , ("swLink",2,swLink)
      , ("gpioLink",2,gpioLink)
      ]
  }

seed :: Module ()
seed = do
  setIdent "Control Logic"
  setSignature "controlLogic"

  led1 <- addPort "LED1" $ do
    swPort
    setType [
        "data" <:= ledData
      , "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port led1 connected
  constrain $ port led1 (typeVal "data.name") :== Lit (StringV "LED1")

  switch1 <- addPort "Switch1" $ do
    swPort
    setType [
        "data" <:= switchData
      , "apiDir" <:= StringV "consumer"
      ]
    return ()

  constrain $ port switch1 connected
  constrain $ port switch1 (typeVal "data.name") :== Lit (StringV "Switch1")

  -- ### Values I'm using to test graphviz output ###

  setType [
      "testInt" <:= IntV 30
    , "testBool" <:= BoolV False
    , "testString" <:= StringV "Testing123!!"
    , "testUID" <:= NewUID
    ]

  r1 <- newResource "testResource1"
  r2 <- newResource "testResource2"
  r3 <- newResource "testResource3"
  r4 <- newResource "testResource4"

  constrainResources "testResourceConstraint1" (Lit $ BoolV True) [
      "tag1" :|= [r1,r2]
    , "tag2" :|= [r2,r3]
    ]

  constrainResources "testResourceConstraint2" (typeVal "testBool") [
      "tag3" :|= [r1,r2,r4]
    , "tag4" :|= [r2,r3,r4]
    ]

  return ()


-- Some general notes about this:
--
-- - We don't keep track of which MCU each piece of software is running
--   on, there's nothing stopping the system from plopping down two MCUs and
--   not realizing there's no way to split the SW across them.
--   Fixing this is left as an exercise for the reader.
main :: IO ()
main = synthesize testLibrary "Seed" seed
