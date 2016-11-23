
module Control.Newtype.Util where

import Control.Newtype
import Control.Applicative

under' :: Newtype n o => (o -> a) -> n -> a
under' f = f . unpack

under2 :: Newtype n o => (o -> o -> b) -> n -> n -> b
under2 f a b = f (unpack a) (unpack b)


-- TODO :: Move the functions below to a more appropriate file
list :: [a] -> [a]
list = id

-- | Lift the lower order checks into a higher order one, since `Nothing`
--   is bottom for this constraint set.
leqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
leqMaybe _ Nothing  _         = True
leqMaybe _ _        Nothing   = False
leqMaybe f (Just a) (Just b ) = f a b

-- | Under the assumption that `nothing == bottom` then this lifts a join
--   on two contraints into a join on maybe those constraints.
joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinMaybe f (Just a) (Just b) = Just $ f a b
joinMaybe _ a b = a <|> b

