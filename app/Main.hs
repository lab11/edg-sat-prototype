module Main where

import Control.Monad
import Control.Monad.Fix
import Data.SBV

import Data.Generics

import Control.Monad.RWS.Lazy

import Data.Map (Map,(!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

-- data IntCons = IGT Integer | ILT Integer | IGTE Integer | ILTE Integer
--   deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)
--
-- type ICMap = Map String IntCons
data IntCons = IGT Integer | ILT Integer | IGTE Integer | ILTE Integer
  deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

type ICMap = Map String IntCons

--
-- newtype MapID = MapID Integer
--   deriving (Eq, Ord, Show, Read)
--
-- type FieldName = String
--
-- type STMap = Map FieldName SInteger
--
-- type PMap = Lnk MapID STMap
--
-- type LoopM a = RWST Oracle [Info] State Symbolic a
--
-- data Oracle = Oracle {
--   oracleIDFields :: Map MapID (Set FieldName)
-- }
--
-- data Info = HasFields MapID (Set FieldName)
--           | LinkMap MapID MapID
--
-- data State = State {
--   stateMapIDCounter :: Integer
-- }
--
--
-- data Lnk first second = Lnk first second
--
-- combinePasses :: FirstPass f d -> SecondPass s d -> LoopM (Lnk f s)
-- combinePasses f s = do
--   (fo,d) <- f
--   so <- s d
--   return $ Lnk fo so
--
-- combinePasses_ :: FirstPass f d -> SecondPass s d -> LoopM ()
-- combinePasses_ f s = const () <$> combinePasses f s
--
-- combinePasses1 :: (fi -> FirstPass fo d)
--                -> (si -> SecondPass so d)
--                -> Lnk fi si -> LoopM (Lnk fo so)
-- combinePasses1 f s (Lnk fi si) = combinePasses (f fi) (s si)
--
-- combinePasses1_ :: (fi -> FirstPass fo d)
--                 -> (si -> SecondPass so d)
--                 -> Lnk fi si -> LoopM ()
-- combinePasses1_ f s (Lnk fi si) = combinePasses_ (f fi) (s si)
--
-- combinePasses2 :: (f1 -> f2 -> FirstPass fo d)
--                -> (s1 -> s2 -> SecondPass so d)
--                -> Lnk f1 s1 -> Lnk f2 s2 -> LoopM (Lnk fo so)
-- combinePasses2 f s (Lnk fi si) = combinePasses1 (f fi) (s si)
--
-- combinePasses2_ :: (f1 -> f2 -> FirstPass fo d)
--                 -> (s1 -> s2 -> SecondPass so d)
--                 -> Lnk f1 s1 -> Lnk f2 s2 -> LoopM ()
-- combinePasses2_ f s (Lnk fi si) = combinePasses1_ (f fi) (s si)
--
--
-- type FirstPassM f = forall m. (MonadWriter [Info] m,MonadState State m)  => m f
-- type FirstPass f d = forall m. (MonadWriter [Info] m,MonadState State m)  => m (f,d)
--
-- type SecondPassM s = forall t. (MonadReader Oracle (t Symbolic),MonadTrans t) => t Symbolic s
-- type SecondPass s d = forall t. (MonadReader Oracle (t Symbolic),MonadTrans t) => d -> t Symbolic s
--
-- runLoopM :: LoopM a -> Symbolic a
-- runLoopM m = mdo
--   (a,_,w) <- runRWST m (convertInfo w) (State 0)
--   return a
-- runLoopM m = undefined
-- --   mdo
-- --   (a,_,w) <- runRWST m (convertInfo w) (State 0)
-- --   return a
--
-- type IDRef = Map MapID (Either MapID (Set FieldName))
--
-- convertInfo :: [Info] -> Oracle
-- convertInfo is = cleanMap $ foldr inf Map.empty is
--   where
--     inf (HasFields i f) m = addFields i f m
--     inf (LinkMap i i') m = forceEq i i' m
--
-- lookupFinal :: MapID -> IDRef -> MapID
-- lookupFinal i m = case Map.lookup i m of
--   Just (Left i') -> lookupFinal i' m
--   _ -> i
--
-- ensureExists :: MapID -> IDRef -> IDRef
-- ensureExists i m = case Map.lookup i m of
--   Just _ -> m
--   Nothing -> Map.insert i (Right Set.empty) m
--
-- addFields :: MapID -> Set FieldName -> IDRef -> IDRef
-- addFields id fs m = Map.adjust adj (lookupFinal id m') m'
--
--   where
--
--   m' = ensureExists id m
--
--   adj (Right s) = Right (Set.union s fs)
--   adj _ = error "There should alwys be a terminating Right in here"
--
-- forceEq :: MapID -> MapID -> IDRef -> IDRef
-- forceEq id id' m = Map.insert idf (Right nfs) . Map.insert id' (Left idf) $ m'
--
--   where
--   idf = lookupFinal id m'
--   Right fs  = m' ! (lookupFinal id m')
--   Right fs' = m' ! (lookupFinal id' m')
--   nfs = Set.union fs fs'
--   m' = ensureExists id . ensureExists id' $ m
--
-- cleanMap :: IDRef -> Oracle
-- cleanMap i = Oracle $ foldr (\ k m -> case i ! (lookupFinal k i) of Right s -> Map.insert k s m) Map.empty (Map.keys i)
--
-- setEqMap :: PMap -> PMap -> LoopM ()
-- setEqMap = combinePasses2_ fp sp
--
--   where
--
--     -- Record the info for later
--     fp :: MapID -> MapID -> FirstPass () ()
--     fp idA idB = const ((),()) <$> tell [LinkMap idA idB]
--
--     -- Generate the neccesary constraints.
--     sp :: STMap -> STMap -> SecondPass () ()
--     sp smA smB _ = forM_ (Map.keys smA) (\ key -> lift . constrain $ (smA ! key) .== (smB ! key))
--
-- setEqVal :: FieldName -> PMap -> PMap -> LoopM ()
-- setEqVal name = combinePasses2_ fp sp
-- -- const () <$> combinePasses (fp idA idB) (sp smA smB)
--
--   where
--
--   fp :: MapID -> MapID -> FirstPass () ()
--   fp idA idB = const ((),()) <$> tell [HasFields idA [name],HasFields idB [name]]
--
--   sp :: STMap -> STMap -> SecondPass () ()
--   sp smA smB _ = lift . constrain $ (smA ! name) .== (smB ! name)
--
-- liftMap :: String -> ICMap -> LoopM PMap
-- liftMap name ic = combinePasses fp sp
--
--   where
--
--   fp :: FirstPass MapID MapID
--   fp = do
--     id <- getNewID
--     tell [HasFields id (Map.keysSet ic)]
--     return (id,id)
--
--   sp :: SecondPass STMap MapID
--   sp id = do
--     Oracle fm <- ask
--     let fields = Set.toList (fm ! id)
--     -- Create the actual sInteger variables and store them in the stmap
--     stmap <- Map.fromList <$> (lift $ forM fields (\ f -> do
--       let vn = name ++ "[" ++ f ++ "]"
--       si <- sInteger vn
--       return (f,si)))
--     -- Apply the constraints to them as needed
--     lift $ forM_ (Map.keys ic) (\ f -> constrain $ addConst (ic ! f) (stmap ! f))
--     -- Return just the stMap
--     return stmap
--
-- addConst :: IntCons -> SInteger -> SBool
-- addConst (IGT  i) s = s .>  (literal i)
-- addConst (IGTE i) s = s .>= (literal i)
-- addConst (ILT  i) s = s .<  (literal i)
-- addConst (ILTE i) s = s .<= (literal i)
--
-- getNewID :: FirstPassM MapID
-- getNewID = do
--   State id <- get
--   put $ State (id + 1)
--   return $ MapID id
--
-- loopProblem :: LoopM SBool
-- loopProblem = do
--   m1' <- liftMap "M1" m1
--   m2' <- liftMap "M2" m2
--   m3' <- liftMap "M3" m3
--   m4' <- liftMap "M4" m4
--   setEqMap m1' m2'
--   setEqVal "C" m3' m4'
--   setEqVal "D" m3' m4'
--   return (true :: SBool)

main :: IO ()
main = print 3

type Forward = RWS Int [Int] Int

data Paired a where
  Pair :: Forward (a,Symbolic ()) -> Paired a

instance Functor Paired where
  fmap f (Pair n) = Pair $ (\ (a,b) -> (f a,b)) <$> n

instance Applicative Paired where
  pure = return
  (<*>) = ap

instance Monad (Paired) where
  return a = Pair $ return (a, return ())

  (>>=) (Pair inp) f = Pair $ do
    (a,s ) <- inp
    let Pair o = f a
    (b,s') <- o
    return (b,s >> s')

-- Class for Forward Pass Monad
--   Assigns IDs to things that need ID
--   Keeps track of type information
--   Assigns values for various properties and gets oracles out.

-- Class for SMT Pass Monad
--   Makes sure all the relevant oracles are correctly typed
--   Make sure we can get information as neccesary

-- Class for pair of Forward and SMT passes
--   Provides the relevant transformation functions, starting state and
--   unwrap/refactor functions. We should probably tag this so that we swap
--   out the transformations we want straightforwardly without too much
--   trouble.


--- Test Data

-- m1 :: ICMap
-- m1 = [("A", IGT 3),  ("B", ILTE 4)]
--
-- m2 :: ICMap
-- m2 = [               ("B", IGT 3), ("C", ILTE 15)]
--
-- m3 :: ICMap
-- m3 = [("A", ILT 13),               ("C", IGTE 12)]
--
-- m4 :: ICMap
-- m4 = [("A", IGT 42), ("B", IGT 6), ("C", ILTE 24), ("D",IGT 25)]

