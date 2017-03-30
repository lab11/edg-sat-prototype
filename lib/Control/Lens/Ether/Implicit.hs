
-- | Control.Lens but with the various state modifying operators changed to
--   make them compatible with tagless ether.
module Control.Lens.Ether.Implicit (
    module Control.Lens
  , uses, use
  , assign, (.=), (%=)
  , (<+=), (<&=)
) where

import Control.Lens           hiding (
    uses, use
  , assign, (.=), (%=), (%%=) , (<%=)
  , (<+=)
  , noneOf
  )
import qualified Control.Lens hiding (
    uses, use
  , assign, (.=), (%=), (%%=)
  , (<+=)
  , noneOf
  )
-- import Control.Monad.Ether.Implicit
import Control.Monad.Ether.Implicit.Writer
import Control.Monad.Ether.Implicit.Except
import Control.Monad.Ether.Implicit.Reader
import Control.Monad.Ether.Implicit.State.Strict

-- * MonadState + Getting

-- | Get an element in the state after modifiying with some function. Do not
--   modify what's in the state.
uses :: (MonadState s m) => LensLike' (Const b) s a -> (a -> b) -> m b
uses l f = gets $ views l f
{-# INLINE uses #-}

-- | Get an element of the state.
use :: (MonadState s m) => Getting a s a -> m a
use = gets . view
{-# INLINE use #-}

-- * MonadState + Setting

assign :: (MonadState s m) => ASetter s s a b -> b -> m ()
assign l v = modify $ (set l v)
{-# INLINE assign #-}

(.=) :: (MonadState s m) => ASetter s s a b -> b -> m ()
(.=) = assign
{-# INLINE (.=) #-}

(%=) :: (MonadState s m) => ASetter s s a b -> (a -> b) -> m ()
(%=) l f = modify $ over l f
{-# INLINE (%=) #-}

-- Given a function to both return an output and change a value, lift it into
-- the state of the current monad.
(<&=) :: (MonadState s m) =>  LensLike ((,) b) s s a a -> (a -> (b,a)) -> m b
(<&=) l f = state $ l f
{-# INLINE (<&=) #-}

-- * MonadState + Math/Num

-- | Given a lens from the state to a number add a value to it, return the
--   new number and modify the state accordingly.
(<+=) :: (Num a, MonadState s m)  => LensLike ((,) a) s s a a -> a -> m a
l <+= a = state $ l <+~ a
{-# INLINE (<+=) #-}


