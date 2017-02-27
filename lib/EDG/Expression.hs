{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module EDG.Expression where

import Data.Foldable (foldrM)

-- TODO :: Split the types for the Exp flag and the monad on expressible
--         by splitting Expressible into two typeclasses.

-- | Type for arbitrary expressions within the system.
--
--   First param is a flag that detmines how we describe values
--   or references
--
data Exp m where
  -- | Literals w/ a name
  Lit :: ExpContext m => ExpLiteral m -> Exp m
  -- | Values in some context
  Val :: ExpContext m => ExpValue m -> Exp m
  -- | Equality Ops
  (:==)   :: Exp m -> Exp m -> Exp m
  (:/=)   :: Exp m -> Exp m -> Exp m
  -- | Boolemn compmrison ops
  (:&&)   :: Exp m -> Exp m -> Exp m
  (:||)   :: Exp m -> Exp m -> Exp m
  (:~&)   :: Exp m -> Exp m -> Exp m
  (:~|)   :: Exp m -> Exp m -> Exp m
  (:<+>)  :: Exp m -> Exp m -> Exp m
  (:=>)   :: Exp m -> Exp m -> Exp m
  Not     :: Exp m -> Exp m
  JustOne :: [Exp m] -> Exp m
  All     :: [Exp m] -> Exp m
  Any     :: [Exp m] -> Exp m
  -- | Ordering Ops
  (:<)  :: Exp m -> Exp m -> Exp m
  (:<=) :: Exp m -> Exp m -> Exp m
  (:>)  :: Exp m -> Exp m -> Exp m
  (:>=) :: Exp m -> Exp m -> Exp m
  -- | Numericml Ops
  (:+)   :: Exp m -> Exp m -> Exp m
  (:-)   :: Exp m -> Exp m -> Exp m
  (:*)   :: Exp m -> Exp m -> Exp m
  (:/)   :: Exp m -> Exp m -> Exp m
  Sum    :: [Exp m] -> Exp m
  Mult   :: [Exp m] -> Exp m
  Negate :: Exp m -> Exp m
  -- | Control Ops
  If :: Exp m -> Exp m -> Exp m -> Exp m
  -- | Other Utility Ops
  -- Returns the number of bool vmlues thmt mre true
  Count :: [Exp m] -> Exp m

-- | Basic instances for the elements
--
--   NOTE :: This is why Undecidable instances is needed
deriving instance (
    ExpContext  m
  , Eq (ExpValue m)
  , Eq (ExpLiteral m)
  ) => Eq (Exp m)
-- deriving instance (
--     ExpContext m
--   , Ord (ExpValue m)
--   , Ord (ExpLiteral m)
--   ) => Ord  (Exp m)
deriving instance (
    ExpContext m
  , Show (ExpValue m)
  , Show (ExpLiteral m)
  ) => Show (Exp m)
deriving instance (
    ExpContext m
  , Read (ExpValue m)
  , Read (ExpLiteral m)
  ) => Read (Exp m)

class (
    Eq (ExpValue m)
  , Eq (ExpLiteral m)
 --  , Ord (ExpValue m)
 --  , Ord (ExpLiteral m)
  , Show (ExpValue m)
  , Show (ExpLiteral m)
  , Read (ExpValue m)
  , Read (ExpLiteral m)
 ) => ExpContext (m :: *) where
  -- The type of the value, and the flag it gets
  type ExpValue   m :: *
  -- The type of a pointer and the flag it gets
  type ExpLiteral m :: *

class (Monad m, ExpContext a) => Expressible a m | a -> m, m -> a where
  -- The intermidate type of an expression that use used as this
  -- system is run.
  type ExpRuntime a = t | t -> a

  -- Neccesary
  intToLit  :: Integer -> m (ExpLiteral a)
  boolToLit :: Bool -> m (ExpLiteral a)

  -- Neccesary
  expressLit :: ExpLiteral a -> m (ExpRuntime a)
  expressVal :: ExpValue   a -> m (ExpRuntime a)

  -- Neccesary
  expressEq  :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNeq :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNeq a b = expressEq a b >>= expressNot

  -- Neccesary
  expressAnd     :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressOr      :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNot     :: ExpRuntime a -> m (ExpRuntime a)

  -- Unneccesary:w
  expressNand :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNand a b = expressAnd a b >>= expressNot
  expressNor :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNor a b = expressOr a b >>= expressNot
  expressXor :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressXor a b = expressEq b =<< expressNot a
  expressImplies :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressImplies a b = expressOr b =<< expressNot a

  -- Unneccesary
  expressJustOne :: [ExpRuntime a] -> m (ExpRuntime a)
  expressJustOne l = do
    one <- i2e 1
    c <- expressCount l
    expressEq one c
  expressAll  :: [ExpRuntime a] -> m (ExpRuntime a)
  expressAll []     = b2e True
  expressAll (t:ts) = foldrM expressAnd t ts
  expressAny :: [ExpRuntime a] -> m (ExpRuntime a)
  expressAny []     = b2e False
  expressAny (t:ts) = foldrM expressOr t ts

  -- Neccesary
  expressLT  :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressLTE :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressGT  :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressGTE :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)

  -- Neccesary
  expressPlus   :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressMinus  :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressTimes  :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressDiv    :: ExpRuntime a -> ExpRuntime a -> m (ExpRuntime a)
  expressNegate :: ExpRuntime a -> m (ExpRuntime a)

  -- Unneccesary
  expressSum :: [ExpRuntime a] -> m (ExpRuntime a)
  expressSum [] = i2e 0
  expressSum (t:ts) = foldrM expressPlus t ts
  expressMult :: [ExpRuntime a] -> m (ExpRuntime a)
  expressMult [] = i2e 1
  expressMult (t:ts) = foldrM expressTimes t ts
  expressCount  :: [ExpRuntime a] -> m (ExpRuntime a)
  expressCount [] = i2e 0
  expressCount ts = mapM ifm ts >>= expressSum
    where
      ifm :: ExpRuntime a -> m (ExpRuntime a)
      ifm t = do
        o <- i2e 1
        z <- i2e 0
        expressIf t o z

  -- Neccesary
  expressIf :: ExpRuntime a -> ExpRuntime a -> ExpRuntime a
            -> m (ExpRuntime a)

  {-# MINIMAL intToLit, boolToLit, expressLit, expressVal, expressEq,
              expressAnd, expressOr, expressNot, expressLT, expressLTE,
              expressGT, expressGTE, expressPlus, expressMinus, expressDiv,
              expressNegate, expressIf, expressTimes #-}

-- | Shorthand for intToLit that wraps things up in an expression
i2e :: forall m a. Expressible a m => Integer -> m (ExpRuntime a)
i2e i = intToLit i >>= expressLit

-- | Shorthand for boolToLit that wraps things up in an expression
b2e :: forall m a. Expressible a m => Bool -> m (ExpRuntime a)
b2e b = boolToLit b >>= expressLit

-- | Unwinds the expression to produce a particular value out, assuming the
--   Expressible typeclass has the neccesary functions defined.
express :: (Expressible a m) => Exp a -> m (ExpRuntime a)
express (Lit l) = expressLit l
express (Val v) = expressVal v
express (a :==  b) = do
  a' <- express a
  b' <- express b
  expressEq a' b'
express (a :/=  b) = do
  a' <- express a
  b' <- express b
  expressNeq a' b'
express (a :&&  b) = do
  a' <- express a
  b' <- express b
  expressAnd a' b'
express (a :||  b) = do
  a' <- express a
  b' <- express b
  expressOr a' b'
express (a :~&  b) = do
  a' <- express a
  b' <- express b
  expressNand a' b'
express (a :~|  b) = do
  a' <- express a
  b' <- express b
  expressNor a' b'
express (a :=>  b) = do
  a' <- express a
  b' <- express b
  expressImplies a' b'
express (a :<+> b) = do
  a' <- express a
  b' <- express b
  expressXor a' b'
express (Not a) = do
  a' <- express a
  expressNot a'
express (JustOne l) = do
  l' <- mapM express l
  expressJustOne l'
express (All l) = do
  l' <- mapM express l
  expressAll l'
express (Any l) = do
  l' <- mapM express l
  expressAny l'
express (a :<  b) =  do
  a' <- express a
  b' <- express b
  expressLT a' b'
express (a :<= b) =  do
  a' <- express a
  b' <- express b
  expressLTE a' b'
express (a :>  b) =  do
  a' <- express a
  b' <- express b
  expressGT a' b'
express (a :>= b) =  do
  a' <- express a
  b' <- express b
  expressGTE a' b'
express (a :+  b) =  do
  a' <- express a
  b' <- express b
  expressPlus a' b'
express (a :-  b) =  do
  a' <- express a
  b' <- express b
  expressMinus a' b'
express (a :*  b) =  do
  a' <- express a
  b' <- express b
  expressTimes a' b'
express (a :/  b) =  do
  a' <- express a
  b' <- express b
  expressDiv a' b'
express (Negate a) =  do
  a' <- express a
  expressNegate a'
express (Sum    l) =  do
  l' <- mapM express l
  expressSum l'
express (Mult   l) =  do
  l' <- mapM express l
  expressMult l'
express (Count  l) =  do
  l' <- mapM express l
  expressCount l'
express (If     c t f) = do
  c' <- express c
  t' <- express t
  f' <- express f
  expressIf c' t' f'

-- | Given that you can transform the leaf nodes, this will transform
--   the entire expression from the bottom up.
convertExpression :: (ExpContext a, ExpContext a')
                  => (ExpLiteral a -> ExpLiteral a')
                  -> (ExpValue a -> ExpValue a')
                  -> Exp a -> Exp a'
convertExpression lconv vconv e
  | Lit l     <- e = Lit $ lconv l
  | Val v     <- e = Val $ vconv v
  | a :== b   <- e = (conv a) :==  (conv b)
  | a :/= b   <- e = (conv a) :/=  (conv b)
  | a :&& b   <- e = (conv a) :&&  (conv b)
  | a :|| b   <- e = (conv a) :||  (conv b)
  | a :~& b   <- e = (conv a) :~&  (conv b)
  | a :~| b   <- e = (conv a) :~|  (conv b)
  | a :<+> b  <- e = (conv a) :<+> (conv b)
  | a :=> b   <- e = (conv a) :=>  (conv b)
  | Not a     <- e = Not $ conv a
  | JustOne l <- e = JustOne $ map conv l
  | All l     <- e = All $ map conv l
  | Any l     <- e = Any $ map conv l
  | a :< b    <- e = (conv a) :<  (conv b)
  | a :<= b   <- e = (conv a) :<= (conv b)
  | a :> b    <- e = (conv a) :>  (conv b)
  | a :>= b   <- e = (conv a) :>= (conv b)
  | a :+ b    <- e = (conv a) :+  (conv b)
  | a :- b    <- e = (conv a) :-  (conv b)
  | a :* b    <- e = (conv a) :*  (conv b)
  | a :/ b    <- e = (conv a) :/  (conv b)
  | Negate a  <- e = Negate $ conv a
  | Sum l     <- e = Sum    $ map conv l
  | Mult l    <- e = Mult   $ map conv l
  | Count l   <- e = Count  $ map conv l
  | If c t f  <- e
    = If (conv c) (conv t) (conv f)
  where
    conv = convertExpression lconv vconv

-- | Monadic version of convertExpression
convertExpressionM :: (ExpContext a, ExpContext a', Monad m)
                  => (ExpLiteral a -> m (ExpLiteral a'))
                  -> (ExpValue a -> m (ExpValue a'))
                  -> Exp a -> m (Exp a')
convertExpressionM lconv vconv e
  | Lit l     <- e = Lit <$> lconv l
  | Val v     <- e = Val <$> vconv v
  | a :== b   <- e = (:==) <$> (conv a) <*> (conv b)
  | a :/= b   <- e = (:/=) <$> (conv a) <*> (conv b)
  | a :&& b   <- e = (:&&) <$> (conv a) <*> (conv b)
  | a :|| b   <- e = (:||) <$> (conv a) <*> (conv b)
  | a :~& b   <- e = (:~&) <$> (conv a) <*> (conv b)
  | a :~| b   <- e = (:~|) <$> (conv a) <*> (conv b)
  | a :<+> b  <- e = (:<+>)<$> (conv a) <*> (conv b)
  | a :=> b   <- e = (:=>) <$> (conv a) <*> (conv b)
  | Not a     <- e = Not <$> conv a
  | JustOne l <- e = JustOne <$> mapM conv l
  | All l     <- e = All <$> mapM conv l
  | Any l     <- e = Any <$> mapM conv l
  | a :< b    <- e = (:< ) <$> (conv a) <*> (conv b)
  | a :<= b   <- e = (:<=) <$> (conv a) <*> (conv b)
  | a :> b    <- e = (:> ) <$> (conv a) <*> (conv b)
  | a :>= b   <- e = (:>=) <$> (conv a) <*> (conv b)
  | a :+ b    <- e = (:+ ) <$> (conv a) <*> (conv b)
  | a :- b    <- e = (:- ) <$> (conv a) <*> (conv b)
  | a :* b    <- e = (:* ) <$> (conv a) <*> (conv b)
  | a :/ b    <- e = (:/ ) <$> (conv a) <*> (conv b)
  | Negate a  <- e = Negate <$> conv a
  | Sum l     <- e = Sum    <$> mapM conv l
  | Mult l    <- e = Mult   <$> mapM conv l
  | Count l   <- e = Count  <$> mapM conv l
  | If c t f  <- e
    = If <$> conv c <*> conv t <*> conv f
  where
    conv = convertExpressionM lconv vconv
