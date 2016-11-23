
module EDG.Classes where

import Algebra.Constrainable



import Text.ParserCombinators.ReadP

import Data.SBV


data FloatConstraints = FloatConstraints {
    fcOneOf       :: Maybe (Set Float)
  , fcNoneOf      :: Maybe (Set Float)
  , fcLessThan    :: Maybe (Float, IsInclusive)
  , fcGreaterThan :: Maybe (Float, IsInclusive)
  } deriving (Show, Read, Eq)

data StrConstraints = SConstraints {
    scOneOf  :: Maybe (Set String)
  , scNoneOf :: Maybe (Set String)
  } deriving (Show, Read, Eq)

data UID = UID Int
  deriving (Show, Read, Eq)

data UIDConstraints = UIDConstraints {
    ucIsNew :: Bool
  } deriving (Show, Read, Eq)

type FieldName = String

data Record f = Record (Map FieldName f)
  deriving (Show, Read, Eq)

data RecordConstraints f = RecContraints {
    rcFieldConstraints :: Map FieldName (Ambiguous f)
  } deriving (Show, Read, Eq)



data TypeVal
  = StrVal String
  | IntVal Int
  | FltVal Float
  | UIDVal UID
  | RecVal (Record TypeVal)
  deriving (Show, Read, Eq)

data TypeValConstraints
  = SVConstraints StrConstraints
  | IVConstraints IntConstraints
  | FVConstraints FloatConstraints
  | UVConstraints UIDConstraints
  | RVConstraints (RecordConstraints TypeVal)
  | Inconsistent
  deriving (Show, Read, Eq)

instance PartialOrd TypeValConstraints where
  leq = undefined

instance JoinSemiLattice TypeValConstraints where
  (\/) = undefined

instance BoundedJoinSemiLattice TypeValConstraints where
  bottom = undefined

instance PartialOrd (Constraints TypeVal) where
  leq = undefined

instance JoinSemiLattice (Constraints TypeVal) where
  (\/) = undefined

instance BoundedJoinSemiLattice (Constraints TypeVal) where
  bottom = undefined

-- I eventually want to be able to defined a recod like this
--
-- rec = ["Port" <~ IntF $ [oneOf [3,4,5], noneOf [4,5,6], greaterThan 2]
--       ,"other" <: StrF $ "Text"
--       ,"thingy" <~ IntF $ any
--       ]

instance Constrainable TypeVal where
  data Constraints TypeVal = Cnsts TypeValConstraints
  validate = undefined
  consistent = undefined
  collapse = undefined
-- Inte
-- String
-- Field
