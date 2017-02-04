
-- | The scribe monad transformer, takes an input monad to do a bunch of
--   actions while "scribing" a set of instructions in another monad, which
--   you can then extract and use.

module Control.Monad.Scribe where

import Control.Newtype

import Control.Monad.Ether.Implicit

-- | Type wrapper that create a monoid using monad sequencing.
newtype SeqMonoid a = SeqMonoid { getSeqMonoid :: a () }

instance Monad a => Newtype (SeqMonoid a) (a ()) where
  pack = SeqMonoid
  unpack = getSeqMonoid

instance Monad a => Monoid (SeqMonoid a) where
  mempty = SeqMonoid $ return ()
  mappend (SeqMonoid a) (SeqMonoid b) = SeqMonoid $ a >> b

-- | Yeah, it's just a writer with the extra newtype wrapper around the
--   written log :V
type ScribeT i = WriterT (SeqMonoid i)

-- | And there's no real interface either, just wrapping the MonadWriter
--   typeclass so that we know we have access to its functions.
--
--   TODO :: Fix that so that the constraint is `MonadScribe i (ScribeT i m)`
--           rather than just `MonadScribe i m`, so as to better fit the
--           interface that mtl uses. This probably means you have to make it
--           an actual typeclass.
--
--           Make sure it works with Ether too.
--
type MonadScribe i m = (Monad i, Monad m, MonadWriter (SeqMonoid i) (ScribeT i m))


-- | Write a new action to the log of things to do.
scribe :: (MonadScribe i m) => i () -> ScribeT i m ()
scribe = tell . SeqMonoid

-- | Run the sribe and get the output plus the instructions.
--
--   In practice it just unwarps the SeqMonoid that runWriterT returns.
--
runScribeT :: (MonadScribe i m) => ScribeT i m a -> m (a, i ())
runScribeT = fmap (fmap unpack) . runWriterT

-- | Exec the scribe, but only return the resulting instructions
--
--   In practice it just unwraps the SeqMonoid that execWriterT returns.
--
execScribeT :: (MonadScribe i m) => ScribeT i m a -> m (i ())
execScribeT = fmap unpack . execWriterT

-- Here's a functionally similar (but for some packing and unpacking) monad
-- that can be used more or less the same way. There's no actual execution
-- difference except that this way I don't have to implement all the various
-- Monad* typeclasses myself.
--
-- Still the following is probably instructive if the above is hard to read.
--
--  > newtype ScribeT i m a = ScribeT { getScribeT :: m (i (),a) }
--  >
--  > instance Newtype (ScribeT i m a) (m (i (),a)) where
--  >   pack = ScribeT
--  >   unpack = getScribeT
--  >
--  > instance Functor m => Functor (ScribeT i m) where
--  >   fmap f = pack . fmap (fmap f) . unpack
--  >
--  > instance (Monad i, Applicative m) => Applicative (ScribeT i m) where
--  >   pure v  = pack $ pure (pure (),v)
--  >   f <*> v = pack $ (\ (i, f) (i', v) -> (i >> i', f v)) <$> unpack f <*> unpack v
--  >
--  > instance (Monad i, Monad m) => Monad (ScribeT i m) where
--  >   return = pure
--  >   k >>= f = pack $ do
--  >     (i ,v) <- unpack k
--  >     (i',o) <- unpack $ f v
--  >     return (i >> i',o)
--  >
--  > instance (Monad i) => MonadTrans (ScribeT i) where
--  >   lift = pack . fmap (return (),)





