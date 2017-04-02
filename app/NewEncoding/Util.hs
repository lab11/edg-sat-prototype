

module NewEncoding.Util where

import EDG
import Control.Monad
import Data.List
import Debug.Trace

-- | Get all unique paris of unequal elements.
allPairs :: Show a => [a] -> [(a,a)]
allPairs l = [(a,b) | (a:ls) <- tails l , b <- ls]

-- | Convinience function that is meant for enforcing that fields in two things
--   are equal modulo some operator. Written to make it easy to use in calls
--   of MapM
constrainBoth :: (IsBlock b, IsElem e, IsElem f)
              => (Exp b -> Exp b -> Exp b)
              -> (Exp e -> Exp b)
              -> (Exp f -> Exp b)
              -> String
              -> b ()
constrainBoth op aTrans bTrans f
  = constrain $ op (aTrans $ typeVal f) (bTrans $ typeVal f)

-- | Set all the fields for each port equal to each other, also optionally
--   include the type of the link as well
setFieldsEq :: (IsBlock b) => Bool -> [PortName] -> [String] -> b ()
setFieldsEq inclBlock ports fields = do
  -- Set these fields equal for every pair of ports
  forM_ (allPairs ports) $ \ (p,p') ->
    mapM_ (constrainBoth (:==) (port p) (port p')) fields
  -- If we're including the blocktype then set that equal too
  when inclBlock . forM_ ports $ \ p ->
    mapM_ (constrainBoth (:==) id (port p)) fields

-- | ensure all the fields specified aren't equal to each other.
setFieldsNeq :: (IsBlock b) => Bool -> [PortName] -> [String] -> b ()
setFieldsNeq inclBlock ports fields = do
  -- Set these fields equal for every pair of ports
  forM_ (allPairs ports) $ \ (p,p') ->
    mapM_ (constrainBoth (:/=) (port p) (port p')) fields
  -- If we're including the blocktype then set that equal too
  when inclBlock . forM_ ports $ \ p ->
    mapM_ (constrainBoth (:/=) id (port p)) fields

-- | Ensure all of these ports are connected
ensureConnected :: (IsBlock b) => [PortName] -> b ()
ensureConnected = mapM_ (\ p -> constrain $ port p connected)

-- | Make sure that the elements in the first pair are equal to the elements
--   in the second pair, but doesn't specify how they are mapped.
--
--   matchedPairs (a,a') (b,b') == (a == b && a' == b') || (a == b' && a' == b)
matchedPairs :: (Constrainable a) => (Exp a, Exp a) -> (Exp a, Exp a) -> Exp a
matchedPairs (a,a') (b,b')
  = ((a :== b) :&& (a' :== b')) :|| ((a :== b') :&& (a' :== b))

-- | Ensure that a particular pair of ports have valid sets of levels for
--   their digital IO
voltageLevelCheck :: (IsBlock p) => PortName -> PortName -> Exp p
voltageLevelCheck source sink =
      (port source (typeVal "0VoltageLevel") :<= port sink (typeVal "limit0VoltageLevel"))
  :&& (port source (typeVal "1VoltageLevel") :>= port sink (typeVal "limit1VoltageLevel"))
