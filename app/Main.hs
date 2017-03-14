module Main where

import EDG

-- ## Simon Example ##

-- | This is a simple example of a microcontroller encoded so that we can
--   perform synthesis over it.
simonMCU :: Module ()
simonMCU = do
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
    , "maxCurrent" <:= FloatC $ greaterThan 0.0
    ]

  -- The 'addPort' function in a module or link allows you to add ports.
  -- The name given is the primary differentiator for ports.
  --
  -- **If you add two ports with the same name the system will see them as
  -- one port with the constraints of both!!**
  addPort "5vOut" $ simonPowerOut "5v" 5.0 0.5  0.5

  -- In the context of a module, you can get a nice identifier for the port
  -- as a return value for the addPort action.
  -- This can be used in place of the name of the port, if you want your code
  -- to be a little more robust.
  p3v3 <- addPort "3v3Out" $ simonPowerOut "3v3" 3.3 0.25 0.25

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
      simonGPIO

      -- Add useful information to the idents.
      appendIdent name

      -- And constrain the type signature a bit more.
      setType [
          "maxCurrent" <:= FloatC $ oneOf [0,3.3] -- Amps
        , "voltage" <:= FloatC $ 0.0 +/- 0.02 -- Volts
        ]

      return ()

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
  constrain $ typeVal "maxCurrent" :>= Sum (
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
simonPowerOut :: forall p. (IsPort p)
              => String -- Identifier prefix
              -> Float -- Voltage
              -> Float -- Error
              -> Float -- Max Current Out
              -> p ()
simonPowerOut identPrefix v vErr maxI = do
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
  setKind "powerOut"

  -- Also they have mandatory types.
  setType [
      "voltage" <:= FloatC $ v +/- vErr
    , "maxCurrent" <:= FloatV maxI
    ]

  -- You can even set constraints over them.
  constrain $ typeVal "voltage" :>= (Lit $ FloatV 0.0 :: Exp p)

  -- These will be expressed in the SMT solver, so prefer constraints in the
  -- types themselves when possible.
  -- For instance these constraints:
  --
  -- > constrain $ typeVal "maxCurrent" :>= (Lit $ FloatV 0.0 )
  -- > constrain $ typeVal "maxCurrent" :<= (Lit $ FloatV 20.0)
  --
  -- Could instead be rendered as:
  --
  setType ["maxCurrent" <:= FloatC [greaterThanEq 0.0, lessThanEq 20.0]]
  --
  -- Which should have the added benefit of informing you that the value is
  -- unusable in the pre-processing state, where there is some debug info.
  -- It's not perfect, but a lot better than a flat "Unsatisfiable" from the
  -- SAT solver.

  -- You can also end a definition for a port, link, or module with a
  -- 'return ()` instead of an endDef.
  -- They are identical, and 'return ()' is probably better practice.
  return ()

-- | A more generic GPIO port definition that we can use as needed elsewhere.
simonGPIO :: (IsPort p) => p ()
simonGPIO = do
  setIdent "GPIO Port"
  setKind "GPIO"
  setType [
      -- 'unknown' is used for elements where we don't know anything about the
      -- value at all. It's the constraint that every value is a member of.
      "maxCurrent" <:= FloatC $ unknown -- Amps

      -- I'm pretty sure we want to split voltage/properties for
      -- multistate pins like this by state.
    , "voltage" <:= FloatC $ unknown -- Volts

    , "direction" <:= StringC $ oneOf ["I","O","IO"]
    ]

  return ()

testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
    modules = [
      ]
  , links   = [
      ]
  }

main :: IO ()
main = synthesize testLibrary "Seed" simonMCU
