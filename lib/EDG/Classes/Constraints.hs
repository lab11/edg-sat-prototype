
module EDG.Classes.Constraints where

import Algebra.Constrainable

type IsInclusive = Bool

class OneOfConstraint t where
  oneOf :: (Constrainable t) => [t] -> Constraints t

is :: (Constrainable t,OneOfConstraint t) =>  t  -> Constraints t
is t = oneOf [t]

class NoneOfConstraint t where
  noneOf :: (Constrainable t) => [t] -> Constraints t

isNot :: (Constrainable t,NoneOfConstraint t) =>  t  -> Constraints t
isNot t = noneOf [t]

class GTConstraint t where
  greaterThan   :: (Constrainable t) => t -> Constraints t
  greaterThanEq :: (Constrainable t) => t -> Constraints t

class LTConstraint t where
  lessThan   :: (Constrainable t) => t -> Constraints t
  lessThanEq :: (Constrainable t) => t -> Constraints t

class UniqConstraint t where
  unique :: (Constrainable t) => t -> Constraints t
