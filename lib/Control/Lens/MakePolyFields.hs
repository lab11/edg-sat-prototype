{-# LANGUAGE CPP #-}

module Control.Lens.MakePolyFields (makePolyFields,makeBarePolyFields,makeBarePolyClass) where

import Control.Lens hiding (LensRules,DefName(..))
import Control.Lens.Internal.TH
import Data.Char
import Data.List
import Data.Maybe
import Text.Show.Pretty (ppShow)

import Control.Monad
import Data.Foldable (toList)
import Data.Maybe (isJust,maybeToList)
import Data.List (nub, findIndices)
import Data.Either (partitionEithers)

import Data.Set.Lens
import           Data.Map ( Map )
import           Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Traversable as T
import Language.Haskell.TH
import Language.Haskell.TH.Lens


{-- Future Inclusion in Control.Lens.TH --}

-- | Generates more polymorphic fields than makeFields, which allow for
--   type changing lenses within the context of fields.
makePolyFields :: Name -> DecsQ
makePolyFields = makeFieldOptics polyFields

-- | Generates the polyfield without the classes, and without the signatures
--   (so that type inference can use whatever
makeBarePolyFields :: Name -> DecsQ
makeBarePolyFields = makeFieldOptics $ polyFields{_generateClasses = False}

-- | Generates just the polyfield class declaration, to allow for declarations
--   of classes in separate modules, etc.
makeBarePolyClass :: Name -> String {- ^ Field Name -} -> DecsQ
makeBarePolyClass opticTypeN fieldName =
  (:[]) <$> makePolyFieldClass (nullOpticStab opticTypeN) className methodName
  where
  -- Just put the correct type in there, otherwise it doesn't matter.
  nullOpticStab t = OpticStab t undefined undefined undefined undefined
  className = mkName $ computeClass fieldName
  methodName = mkName $ computeMethod fieldName

polyFields :: LensRules
polyFields = LensRules
  { _simpleLenses    = False -- !!! PolyFields will be built iff complex lenses
                             -- !!! are enabled, classyLendes returns Nothing
                             -- !!! and there is a field to be built.
  , _generateSigs    = True
  , _generateClasses = True  -- classes will still be skipped if they already exist
  , _allowIsos       = False -- generating Isos would hinder field class reuse
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = camelCaseNamer
  }

-- Given Field Part compute the method name
computeMethod :: String -> String
computeMethod = overHead toLower

-- Given Field Part compute the class name
computeClass :: String -> String
computeClass = ("Has" ++)

camelCaseNamer :: Name      -> [Name]      -> Name              -> [DefName]
             -- ^ Type Name -> Field Names -> Target Field Name -> Definition Names
camelCaseNamer tyName fields field = maybeToList $ do

  fieldPart <- stripPrefix expectedPrefix (nameBase field)
  let (x:xs) = fieldPart
  guard $ isUpper x
  let method = computeMethod fieldPart
  let cls = computeClass fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  expectedPrefix = optUnderscore ++ overHead toLower (nameBase tyName)

  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]


{-- Future Inclusion in Control.Lens.Internal.FieldTH --}

-- | Compute the field optics for a deconstructed Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldOpticsForDec' :: LensRules -> Name -> Type -> [Con] -> DecsQ
makeFieldOpticsForDec' rules tyName s cons =
  do fieldCons <- traverse normalizeConstructor cons
     let allFields  = toListOf (folded . _2 . folded . _1 . folded) fieldCons
     let defCons    = over normFieldLabels (expandName allFields) fieldCons
         allDefs    = setOf (normFieldLabels . folded) defCons
     perDef <- T.sequenceA (fromSet (buildScaffold rules s defCons) allDefs)

     let defs = Map.toList perDef
     case _classyLenses rules tyName of
       Just (className, methodName) ->
         makeClassyDriver rules className methodName s defs
       Nothing -> do decss  <- traverse (makeOptic rules) defs --- !!!
                     return (concat decss)

  where

  -- Determine whether to use the polyfield optic function or the generic
  -- field optic function
  makeOptic :: LensRules -> (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) -> DecsQ -- !!!
  makeOptic | _simpleLenses rules = makeFieldOptic
            | otherwise           = makePolyFieldOptic

  -- Traverse the field labels of a normalized constructor
  normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
  normFieldLabels = traverse . _2 . traverse . _1

  -- Map a (possibly missing) field's name to zero-to-many optic definitions
  expandName :: [Name] -> Maybe Name -> [DefName]
  expandName allFields = concatMap (_fieldToDef rules tyName allFields) . maybeToList

-- | Compute the positional location of the fields involved in
-- each constructor for a given optic definition as well as the
-- type of clauses to generate and the type to annotate the declaration
-- with.
buildScaffold ::
  LensRules                                                                  ->
  Type                              {- ^ outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- ^ normalized constructors          -} ->
  DefName                           {- ^ target definition                -} ->
  Q (OpticType, OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold rules s cons defName =

  do (s',t,a,b) <- buildStab s (concatMap snd consForDef)

     let defType
           | Just (_,cx,a') <- preview _ForallT a =
               let optic | lensCase  = getterTypeName
                         | otherwise = foldTypeName
               in OpticSa cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase  = getterTypeName
                         | otherwise = foldTypeName
               in OpticSa [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules = --- !!!
               let optic | isoCase && _allowIsos rules = iso'TypeName
                         | lensCase                    = lens'TypeName
                         | otherwise                   = traversal'TypeName
               in OpticSa [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic | isoCase && _allowIsos rules = isoTypeName
                         | lensCase                    = lensTypeName
                         | otherwise                   = traversalTypeName
               in OpticStab optic s' t a b

         opticType | has _ForallT a            = GetterType
                   | not (_allowUpdates rules) = GetterType
                   | isoCase                   = IsoType
                   | otherwise                 = LensType

     return (opticType, defType, scaffolds)
  where
  consForDef :: [(Name, [Either Type Type])]
  consForDef = over (mapped . _2 . mapped) categorize cons

  scaffolds :: [(Name, Int, [Int])]
  scaffolds = [ (n, length ts, rightIndices ts) | (n,ts) <- consForDef ]

  rightIndices :: [Either Type Type] -> [Int]
  rightIndices = findIndices (has _Right)

  -- Right: types for this definition
  -- Left : other types
  categorize :: ([DefName], Type) -> Either Type Type
  categorize (defNames, t)
    | defName `elem` defNames = Right t
    | otherwise               = Left  t

  lensCase :: Bool
  lensCase = all (\x -> lengthOf (_2 . folded . _Right) x == 1) consForDef

  isoCase :: Bool
  isoCase = case scaffolds of
              [(_,1,[0])] -> True
              _           -> False


-- | Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makePolyFieldOptic ::
  LensRules ->
  (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) ->
  DecsQ
makePolyFieldOptic rules (defName, (opticType, defType, cons)) =
  do cls <- mkCls
     T.sequenceA (cls ++ sig ++ def)
  where
  mkCls = case defName of
          MethodName c n | _generateClasses rules ->
            do classExists <- isJust <$> lookupTypeName (show c)
               return (if classExists then [] else [makePolyFieldClass defType c n])
          _ -> return []

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n clauses : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makePolyFieldInstance defType c (fun n)]

  clauses = makeFieldClauses rules opticType cons

makePolyFieldClass :: OpticStab -> Name -> Name -> DecQ
makePolyFieldClass defType className methodName =
  classD (cxt []) className [PlainTV s, PlainTV t, PlainTV a, PlainTV b] [FunDep [s] [a],FunDep [t] [b],FunDep [s,b] [t],FunDep [t,a] [s]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,t,a,b])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT t,VarT a,VarT b]
  s = mkName "s"
  t = mkName "t"
  a = mkName "a"
  b = mkName "b"

makePolyFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makePolyFieldInstance (OpticStab _ s t a b) className =
  instanceD (cxt [])
    (return (className `conAppsT` [s,t,a,b]))


-------------------------------------------------

{-- Unmodified components of Control.Lens.TH --}

overHead _ []     = []
overHead f (x:xs) = f x : xs

{-- Unmodified components of Control.Lens.Internal.FieldTH --}

------------------------------------------------------------------------
-- Field generation entry point
------------------------------------------------------------------------


-- | Compute the field optics for the type identified by the given type name.
-- Lenses will be computed when possible, Traversals otherwise.
makeFieldOptics :: LensRules -> Name -> DecsQ
makeFieldOptics rules tyName =
  do info <- reify tyName
     case info of
       TyConI dec -> makeFieldOpticsForDec rules dec
       _          -> fail "makeFieldOptics: Expected type constructor name"


makeFieldOpticsForDec :: LensRules -> Dec -> DecsQ
makeFieldOpticsForDec rules dec = case dec of
  DataD    _ tyName vars cons _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) cons
  NewtypeD _ tyName vars con  _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) [con]
  DataInstD _ tyName args cons _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) cons
  NewtypeInstD _ tyName args con _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) [con]
  _ -> fail "makeFieldOptics: Expected data or newtype type-constructor"
  where
  mkS tyName vars = tyName `conAppsT` map VarT (toListOf typeVars vars)

-- | Normalized the Con type into a uniform positional representation,
-- eliminating the variance between records, infix constructors, and normal
-- constructors.
normalizeConstructor ::
  Con ->
  Q (Name, [(Maybe Name, Type)]) -- ^ constructor name, field name, field type

normalizeConstructor (RecC n xs) =
  return (n, [ (Just fieldName, ty) | (fieldName,_,ty) <- xs])

normalizeConstructor (NormalC n xs) =
  return (n, [ (Nothing, ty) | (_,ty) <- xs])

normalizeConstructor (InfixC (_,ty1) n (_,ty2)) =
  return (n, [ (Nothing, ty1), (Nothing, ty2) ])

normalizeConstructor (ForallC _ _ con) =
  do con' <- normalizeConstructor con
     return (set (_2 . mapped . _1) Nothing con')



data OpticType = GetterType | LensType | IsoType deriving (Show)


data OpticStab = OpticStab     Name Type Type Type Type
               | OpticSa   Cxt Name Type Type
               deriving(Show)

stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) = quantifyType [] (c `conAppsT` [s,t,a,b])
stabToType (OpticSa cx c s   a  ) = quantifyType cx (c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}        = []
stabToContext (OpticSa cx _ _ _) = cx

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = c
stabToOptic (OpticSa _ c _ _) = c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ a) = a

-- | Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab s categorizedFields =
  do (subA,a) <- unifyTypes targetFields
     let s' = applyTypeSubst subA s

     -- compute possible type changes
     sub <- T.sequenceA (fromSet (newName . nameBase) unfixedTypeVars)
     let (t,b) = over both (substTypeVars sub) (s',a)

     return (s',t,a,b)

  where
  (fixedFields, targetFields) = partitionEithers categorizedFields
  fixedTypeVars               = setOf typeVars fixedFields
  unfixedTypeVars             = setOf typeVars s Set.\\ fixedTypeVars


-- | Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makeFieldOptic ::
  LensRules ->
  (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) ->
  DecsQ
makeFieldOptic rules (defName, (opticType, defType, cons)) =
  do cls <- mkCls
     T.sequenceA (cls ++ sig ++ def)
  where
  mkCls = case defName of
          MethodName c n | _generateClasses rules ->
            do classExists <- isJust <$> lookupTypeName (show c)
               return (if classExists then [] else [makeFieldClass defType c n])
          _ -> return []

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n clauses : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makeFieldInstance defType c (fun n)]

  clauses = makeFieldClauses rules opticType cons


------------------------------------------------------------------------
-- Classy class generator
------------------------------------------------------------------------


makeClassyDriver ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecsQ
makeClassyDriver rules className methodName s defs = T.sequenceA (cls ++ inst)

  where
  cls | _generateClasses rules = [makeClassyClass className methodName s defs]
      | otherwise = []

  inst = [makeClassyInstance rules className methodName s defs]


makeClassyClass ::
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyClass className methodName s defs = do
  let ss   = map (stabToS . view (_2 . _2)) defs
  (sub,s') <- unifyTypes (s : ss)
  c <- newName "c"
  let vars = toListOf typeVars s'
      fd   | null vars = []
           | otherwise = [FunDep [c] vars]


  classD (cxt[]) className (map PlainTV (c:vars)) fd
    $ sigD methodName (return (lens'TypeName `conAppsT` [VarT c, s']))
    : concat
      [ [sigD defName (return ty)
        ,valD (varP defName) (normalB body) []
        ] ++
        inlinePragma defName
      | (TopName defName, (_, stab, _)) <- defs
      , let body = appsE [varE composeValName, varE methodName, varE defName]
      , let ty   = quantifyType' (Set.fromList (c:vars))
                                 (stabToContext stab)
                 $ stabToOptic stab `conAppsT`
                       [VarT c, applyTypeSubst sub (stabToA stab)]
      ]


makeClassyInstance ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyInstance rules className methodName s defs = do
  methodss <- traverse (makeFieldOptic rules') defs

  instanceD (cxt[]) (return instanceHead)
    $ valD (varP methodName) (normalB (varE idValName)) []
    : map return (concat methodss)

  where
  instanceHead = className `conAppsT` (s : map VarT vars)
  vars         = toListOf typeVars s
  rules'       = rules { _generateSigs    = False
                       , _generateClasses = False
                       }

------------------------------------------------------------------------
-- Field class generation
------------------------------------------------------------------------

makeFieldClass :: OpticStab -> Name -> Name -> DecQ
makeFieldClass defType className methodName =
  classD (cxt []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,a])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"

makeFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makeFieldInstance defType className =
  instanceD (cxt [])
    (return (className `conAppsT` [stabToS defType, stabToA defType]))

------------------------------------------------------------------------
-- Optic clause generators
------------------------------------------------------------------------


makeFieldClauses :: LensRules -> OpticType -> [(Name, Int, [Int])] -> [ClauseQ]
makeFieldClauses rules opticType cons =
  case opticType of

    IsoType    -> [ makeIsoClause conName | (conName, _, _) <- cons ]

    GetterType -> [ makeGetterClause conName fieldCount fields
                    | (conName, fieldCount, fields) <- cons ]

    LensType   -> [ makeFieldOpticClause conName fieldCount fields irref
                    | (conName, fieldCount, fields) <- cons ]
      where
      irref = _lazyPatterns rules
           && length cons == 1



-- | Construct an optic clause that returns an unmodified value
-- given a constructor name and the number of fields on that
-- constructor.
makePureClause :: Name -> Int -> ClauseQ
makePureClause conName fieldCount =
  do xs <- replicateM fieldCount (newName "x")
     -- clause: _ (Con x1..xn) = pure (Con x1..xn)
     clause [wildP, conP conName (map varP xs)]
            (normalB (appE (varE pureValName) (appsE (conE conName : map varE xs))))
            []


-- | Construct an optic clause suitable for a Getter or Fold
-- by visited the fields identified by their 0 indexed positions
makeGetterClause :: Name -> Int -> [Int] -> ClauseQ
makeGetterClause conName fieldCount []     = makePureClause conName fieldCount
makeGetterClause conName fieldCount fields =
  do f  <- newName "f"
     xs <- replicateM (length fields) (newName "x")

     let pats (i:is) (y:ys)
           | i `elem` fields = varP y : pats is ys
           | otherwise = wildP : pats is (y:ys)
         pats is     _  = map (const wildP) is

         fxs   = [ appE (varE f) (varE x) | x <- xs ]
         body  = foldl (\a b -> appsE [varE apValName, a, b])
                       (appE (varE coerceValName) (head fxs))
                       (tail fxs)

     -- clause f (Con x1..xn) = coerce (f x1) <*> ... <*> f xn
     clause [varP f, conP conName (pats [0..fieldCount - 1] xs)]
            (normalB body)
            []

-- | Build a clause that updates the field at the given indexes
-- When irref is 'True' the value with me matched with an irrefutable
-- pattern. This is suitable for Lens and Traversal construction
makeFieldOpticClause :: Name -> Int -> [Int] -> Bool -> ClauseQ
makeFieldOpticClause conName fieldCount [] _ =
  makePureClause conName fieldCount
makeFieldOpticClause conName fieldCount (field:fields) irref =
  do f  <- newName "f"
     xs <- replicateM fieldCount          (newName "x")
     ys <- replicateM (1 + length fields) (newName "y")

     let xs' = foldr (\(i,x) -> set (ix i) x) xs (zip (field:fields) ys)

         mkFx i = appE (varE f) (varE (xs !! i))

         body0 = appsE [ varE fmapValName
                       , lamE (map varP ys) (appsE (conE conName : map varE xs'))
                       , mkFx field
                       ]

         body = foldl (\a b -> appsE [varE apValName, a, mkFx b]) body0 fields

     let wrap = if irref then tildeP else id

     clause [varP f, wrap (conP conName (map varP xs))]
            (normalB body)
            []


-- | Build a clause that constructs an Iso
makeIsoClause :: Name -> ClauseQ
makeIsoClause conName = clause [] (normalB (appsE [varE isoValName, destruct, construct])) []
  where
  destruct  = do x <- newName "x"
                 lam1E (conP conName [varP x]) (varE x)

  construct = conE conName


------------------------------------------------------------------------
-- Unification logic
------------------------------------------------------------------------

-- The field-oriented optic generation supports incorporating fields
-- with distinct but unifiable types into a single definition.



-- | Unify the given list of types, if possible, and return the
-- substitution used to unify the types for unifying the outer
-- type when building a definition's type signature.
unifyTypes :: [Type] -> Q (Map Name Type, Type)
unifyTypes (x:xs) = foldM (uncurry unify1) (Map.empty, x) xs
unifyTypes []     = fail "unifyTypes: Bug: Unexpected empty list"


-- | Attempt to unify two given types using a running substitution
unify1 :: Map Name Type -> Type -> Type -> Q (Map Name Type, Type)
unify1 sub (VarT x) y
  | Just r <- Map.lookup x sub = unify1 sub r y
unify1 sub x (VarT y)
  | Just r <- Map.lookup y sub = unify1 sub x r
unify1 sub x y
  | x == y = return (sub, x)
unify1 sub (AppT f1 x1) (AppT f2 x2) =
  do (sub1, f) <- unify1 sub  f1 f2
     (sub2, x) <- unify1 sub1 x1 x2
     return (sub2, AppT (applyTypeSubst sub2 f) x)
unify1 sub x (VarT y)
  | elemOf typeVars y (applyTypeSubst sub x) =
      fail "Failed to unify types: occurs check"
  | otherwise = return (Map.insert y x sub, x)
unify1 sub (VarT x) y = unify1 sub y (VarT x)

-- TODO: Unify contexts
unify1 sub (ForallT v1 [] t1) (ForallT v2 [] t2) =
     -- This approach works out because by the time this code runs
     -- all of the type variables have been renamed. No risk of shadowing.
  do (sub1,t) <- unify1 sub t1 t2
     v <- fmap nub (traverse (limitedSubst sub1) (v1++v2))
     return (sub1, ForallT v [] t)

unify1 _ x y = fail ("Failed to unify types: " ++ show (x,y))


-- | Perform a limited substitution on type variables. This is used
-- when unifying rank-2 fields when trying to achieve a Getter or Fold.
limitedSubst :: Map Name Type -> TyVarBndr -> Q TyVarBndr
limitedSubst sub (PlainTV n)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (PlainTV m)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst sub (KindedTV n k)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (KindedTV m k)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst _ tv = return tv


-- | Apply a substitution to a type. This is used after unifying
-- the types of the fields in unifyTypes.
applyTypeSubst :: Map Name Type -> Type -> Type
applyTypeSubst sub = rewrite aux
  where
  aux (VarT n) = Map.lookup n sub
  aux _        = Nothing


------------------------------------------------------------------------
-- Field generation parameters
------------------------------------------------------------------------


data LensRules = LensRules
  { _simpleLenses    :: Bool
  , _generateSigs    :: Bool
  , _generateClasses :: Bool
  , _allowIsos       :: Bool
  , _allowUpdates    :: Bool -- ^ Allow Lens/Traversal (otherwise Getter/Fold)
  , _lazyPatterns    :: Bool
  , _fieldToDef      :: Name -> [Name] -> Name -> [DefName]
       -- ^ Type Name -> Field Names -> Target Field Name -> Definition Names
  , _classyLenses    :: Name -> Maybe (Name,Name)
       -- type name to class name and top method
  }


-- | Name to give to generated field optics.
data DefName
  = TopName Name -- ^ Simple top-level definiton name
  | MethodName Name Name -- ^ makeFields-style class name and method name
  deriving (Show, Eq, Ord)

------------------------------------------------------------------------
-- Miscellaneous utility functions
------------------------------------------------------------------------


-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t))

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t Set.\\ exclude))


------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]

#ifdef INLINING

#if MIN_VERSION_template_haskell(2,8,0)

# ifdef OLD_INLINE_PRAGMAS
-- 7.6rc1?
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase Inline False)]
# else
-- 7.7.20120830
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
# endif

#else
-- GHC <7.6, TH <2.8.0
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase True False)]
#endif

#else

inlinePragma _ = []

#endif

{-
makePolyFieldClass :: Name {- ^ Use Lens or Traversal? -}
                   -> Name {- ^ Name of Class -}
                   -> Name {- ^ Name of methodWithinClass -}
                   -> Type {- ^ Outer S Type -}
                   -> [(DefName, (OpticType, OpticStab, [(Name,Int, [Int])]))]
                   -> DecQ
makePolyFieldClass = undefined
-}


{--
--  -
-- genClassName      = mkName . ("Has" ++)    . over (ix 0) toUpper
-- genLensName       = mkName .                 over (ix 0) toLower
--
-- -- | Generate a polymorphic class declaration for a particular field, in
-- --   particular for some field "var" generate the following.
-- --   The second declaration is to make a total version of the lens when some
-- --   constructors for the type might not have the particular value.
-- --
-- -- > class HasVar s t a b | s -> a, t -> b, s b -> t, t a -> s where
-- -- >   var :: Lens s t a b
-- --
-- -- Sorry for the complexity here, the function just
-- --   gets unique type variable names for 's','t','a', and 'b',
-- --   generates the class and lens names,
-- --   and assembles the AST of the above snippet.
-- makePolyFieldClass :: String -> DecsQ
-- makePolyFieldClass name =
--   do typeS <- newName "s"
--      typeT <- newName "t"
--      typeA <- newName "a"
--      typeB <- newName "b"
--      let typeVars = map PlainTV [typeS,typeT,typeA,typeB]
--      let fDeps    = funDeps typeS typeT typeA typeB
--      let lSig     = lensSig $ map VarT [typeS,typeT,typeA,typeB]
--      return [ClassD [] className typeVars fDeps [lSig{--,lmSig--}]]
--   where className          = genClassName     name
--         lensName           = genLensName      name
--         toMaybe t          = AppT (ConT (mkName "Maybe")) (VarT t)
--         lensType     tVars = foldl AppT (ConT lensTypeName) tVars
--         lensSig      tVars = SigD lensName      (lensType tVars)
--         funDeps s t a b    = zipWith FunDep [[s], [t], [s,b], [t,a]]
--                                             [[a], [b], [t]  , [s]  ]
--
-- -- [^1]: Doesn't typecheck without the explicit annotation, not sure why :/
--
-- eqN :: Name -> Name -> Bool
-- eqN a b = (nameBase a) == (nameBase b)
--
-- getName :: TyVarBndr -> Name
-- getName (PlainTV  name)   = name
-- getName (KindedTV name _) = name
--
-- -- | Perform a single type variable replacement using equality of Name
-- replaceTypeVar :: Type -> (Name,Name) -> Type
-- replaceTypeVar   (ForallT l c t) p     = ForallT l c (replaceTypeVar t p)
-- replaceTypeVar   (AppT t1 t2)    p     = AppT (replaceTypeVar t1 p) (replaceTypeVar t2 p)
-- replaceTypeVar   (SigT t k)      p     = SigT (replaceTypeVar t p) k
-- replaceTypeVar t@(VarT n)        (o,r) = if n `eqN` o then VarT r else VarT n
-- replaceTypeVar t                 _     = t
--
-- -- | Perform a series of type variable replacements
-- replaceTypeVars :: Type -> [(Name,Name)] -> Type
-- replaceTypeVars = foldl replaceTypeVar
--
-- -- | Given a fieldname and a single constructor get the type of the field,
-- --   iff a field with that name exists.
-- getConFieldType :: Name -> Con -> Maybe Type
-- getConFieldType _         (NormalC _ _)       = Nothing
-- getConFieldType _         (InfixC _ _ _)      = Nothing
-- getConFieldType fieldName (ForallC tvs _ con) = error "I don't know what this should do"
-- getConFieldType fieldName (RecC _ tvs)        = fmap (view _3) (find isCorrectField tvs)
--   where isCorrectField (n,_,_) = n `eqN` fieldName
--
--
-- -- | Makes sure that there is a single type associated with the field type
-- --   among a set of constructors and returns it if possible.
-- getConsFieldType :: [Con] -> Name -> Q Type
-- getConsFieldType cl fieldName = case fieldTypes of
--                                   a:[] -> return a
--                                   _    -> fail $ "Type resolution of field "
--                                                  ++ (nameBase fieldName)
--                                                  ++ " failed with "
--                                                  ++ (show fieldTypes)
--   where fieldTypes = nub $ mapMaybe (getConFieldType fieldName) cl
--
-- makePolyFieldInstanceStub :: String -> Name -> [Name] -> Name -> DecsQ
-- makePolyFieldInstanceStub a b c decN =
--   do info <- reify decN
--      case info of
--        TyConI d    -> makePolyFieldInstance a b c d
--        t           -> fail $ "Declaration Name Resovled to " ++ (show t)
--
-- -- | For a given data declaration, className, field, and variables to be
-- --   polymorphic in, generate a valid isntance of the "HasName" class.
-- --
-- --   Issues: Doesn't deal with existential quantifiers properly, and dealing
-- --           with multiple constructors is likely broken.
-- makePolyFieldInstance :: String -> Name -> [Name] -> Dec -> DecsQ
-- makePolyFieldInstance name fieldName polyVars d@(DataD _ typeName typeVars constructors _)
--   = do polyPairs <- mapM genNamePair polyVars -- Get New Replacements for Polymorphic Variables
--        reportError $ ppShow (name,fieldName,polyVars,d)
--        let typeS = foldl AppT (ConT typeName) $ map (VarT . getName) typeVars
--        let typeT = replaceTypeVars typeS polyPairs  -- S with all the pairs replaced properly
--        typeA <- getConsFieldType constructors fieldName
--        let typeB = replaceTypeVars typeA polyPairs -- A with all the pairs replaced properly
--        let instanceType = classType typeS typeT typeA typeB
--        clauses <- mapM genClause constructors
--        let lensFun = FunD lensName clauses
--        return [InstanceD [] instanceType [lensFun]]
--   where className = genClassName name
--         lensName  = genLensName name
--         genNamePair o = fmap (o,) $ newName (nameBase o)
--         classType s t a b = foldl AppT (ConT className) ([s,t,a,b] :: [Type])
--         fmapE = VarE $ mkName "fmap"
--         errorExpr consN =  AppE (VarE $ mkName "error")
--                                 (LitE . StringL $ "Cannot use lens " ++ name
--                                                   ++ " with constructor "
--                                                   ++ (nameBase consN)
--                                                   ++ " as it has no corresponding field.")
--         errorClause consN consP = Clause [WildP,consP] (NormalB $ errorExpr consN) []
--         genNamePairVST (n,_,_) = do pN <- newName (nameBase n)
--                                     pN' <- if n `eqN` fieldName
--                                            then newName (nameBase n)
--                                            else return pN
--                                     return (pN,pN')
--         genClause (RecC consN typeList) = do fN <- newName "f"
--                                              namePairs <- mapM genNamePairVST typeList
--                                              let (origNs,replNs) = unzip namePairs
--                                              let consP = ConP consN $ map VarP origNs
--                                              let subComp = find (uncurry eqN) namePairs
--                                              case subComp of
--                                                Nothing -> return $ errorClause consN consP
--                                                Just (subtermN,repN) ->
--                                                  do let fP = VarP fN
--                                                     let fE = VarE fN
--                                                     let subtermE = VarE subtermN
--                                                     let insE = foldl AppE (ConE consN) $ map VarE replNs
--                                                     let setE = LamE [VarP repN] insE
--                                                     let body = NormalB $ AppE (AppE fmapE setE)
--                                                                               (AppE fE subtermE)
--                                                     return $ Clause [fP,consP] body []
-- makePolyFieldInstance n fn pv nt@(NewtypeD _ _ _ _ _) = makePolyFieldInstance n fn pv $ deNewtype nt
-- makePolyFieldInstance name _ _ _
--   = fail $ "Cannot generate field instance for " ++ name ++ " without data or newtype declaration."
--
-- -- | Transform @NewtypeD@s declarations to @DataD@s.
-- deNewtype :: Dec -> Dec
-- deNewtype (NewtypeD ctx tyConName args c d) = DataD ctx tyConName args [c] d
-- deNewtype d = d
--
-- -- | Pull a single declaration from a name when possible. Usually will then
-- --   match against the particular declaration type needed in the calling function.
-- getDeclFromName :: Name -> Q Dec
-- getDeclFromName n =
--   do info <- reify n
--      case info of
--        ClassI  d _ -> return d
--        TyConI  d   -> return d
--        FamilyI d _ -> return d
--        _           -> fail $ "No unique decl could be retieved from " ++ (nameBase n) ++ " ."
--
-- getTypeVarsFromType :: Type -> [Name]
-- getTypeVarsFromType (ForallT tvl _ t) = filter (\ n -> notElem n polyVars) (getTypeVarsFromType t)
--   where polyVars = map getName tvl
-- getTypeVarsFromType (SigT t _) = getTypeVarsFromType t
-- getTypeVarsFromType (AppT t1 t2) = (getTypeVarsFromType t1) ++ (getTypeVarsFromType t2)
-- getTypeVarsFromType (VarT n) = [n]
-- getTypeVarsFromType t = []
--
-- getTypeVarSetsFromCon :: Con -> [(Maybe Name,[Name])]
-- getTypeVarSetsFromCon (NormalC _ st) = [(Nothing,concatMap (getTypeVarsFromType . snd) st)]
-- getTypeVarSetsFromCon (RecC _ vst) = map (\ (n,_,t) -> (Just n, getTypeVarsFromType t)) vst
-- getTypeVarSetsFromCon (InfixC t1 _ t2) = [(Nothing, concatMap (getTypeVarsFromType . snd) ([t1,t2] :: [(Language.Haskell.TH.Strict,Type)]))]
-- getTypeVarSetsFromCon (ForallC tvl _ c) = undefined
--
--
--
-- unifyPairLists :: (Ord a,Eq b) => [(a,[b])] -> [(a,[b])]
-- unifyPairLists = map extractFst . groupBy eqFst . sortOn fst
--   where eqFst (a,_) (b,_) = a == b
--         extractFst l@((a,_):_) = (a,nub . concatMap snd $ l)
--
-- getTypeVarSetsFromCons :: [Con] -> [(Maybe Name,[Name])]
-- getTypeVarSetsFromCons = unifyPairLists . concatMap getTypeVarSetsFromCon
--
-- select [] = []
-- select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]
--
-- findPolyVars :: [(Maybe Name,[Name])] -> [(Maybe Name,[Name])]
-- findPolyVars = map filterDuplicates . map nameSets . select
--   where nameSets ((n,v),l) = (n,v,nub . concatMap snd $ l)
--         filterDuplicates (n,v,l) = (n, filter (\ d -> notElem d l) v)
--
-- removeNothingFst :: [(Maybe a,b)] -> [(a,b)]
-- removeNothingFst = concatMap getElem
--   where getElem (Just a,b) = [(a,b)]
--         getElem (Nothing,_) = []
--
-- getNameClasses :: Name -> [(Name,a)] -> [(Name,String,a)]
-- getNameClasses tn fns =
--   do (fieldName,meta) <- fns
--      classNameS <- stripPrefix $ nameBase fieldName
--      return (fieldName,classNameS,meta)
--   where typeName = over (ix 0) toLower $ nameBase tn
--         useUnderscore = any (isPrefixOf "_") $ map (nameBase . fst) fns
--         prefix = if useUnderscore then "_" else "" ++ typeName
--         stripPrefix = maybeToList . Data.List.stripPrefix prefix
--
--
-- -- | Creates all the relevant isntances
-- makePolyField :: Name -> DecsQ
-- makePolyField decN =
--   do dat@(DataD _ typeName typeVars constructors _) <- deNewtype <$> getDeclFromName decN
--      let namePolyPairs = getNameClasses typeName . removeNothingFst . findPolyVars . getTypeVarSetsFromCons $ constructors
--      -- fail $ show $ namePolyPairs
--      concat <$> mapM (\ (fn,cn,pp) -> makePolyFieldInstance cn fn pp dat) namePolyPairs
--
--
--
-- -- | Takes the name of an available data or newtype declaration and creates the
-- --   corresponding instances. It doesn't make the class definitions however,
-- --   that's manual/separate for the moment.
-- makePolyFieldInstances :: Name -> DecsQ
-- makePolyFieldInstances dataDeclName = undefined
--
--}
