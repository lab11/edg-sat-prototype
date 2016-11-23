
module Control.Newtype.Util where

import Control.Newtype

under' :: Newtype n o => (o -> a) -> n -> a
under' f = f . unpack

under2 :: Newtype n o => (o -> o -> b) -> n -> n -> b
under2 f a b = f (unpack a) (unpack b)


-- TODO :: Move the functions below to a more appropriate file
list :: [a] -> [a]
list = id
