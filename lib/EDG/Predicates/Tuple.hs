
module EDG.Predicates.Tuple where

import Algebra.AsPredicate

import Control.Newtype.Util

-- All of the tuples here are another convinience instance, that are mainly
-- used to define how the conjunctions of multiple disparate predicates over
-- the same domain are definined. Namely by their join.
--
-- We also carefully refrain from defining any other instances since things
-- like how join occour may involve interactions between multiple elements of
-- the tuple, and should be determined on a case by case basis so that
-- instances of Eq and the like are satisfied.
--
-- All of these instances are identical, all elements of the tuple must be
-- preicates over the same domain, and the entire tuple's predicate is the
-- join over each induvidual predicate.

instance (AsPredicate a, AsPredicate b
         ,PredDom a ~ PredDom b)
         => AsPredicate (a,b) where
  type PredicateDomain (a,b) = PredDom a
  asPredicate (a,b) t = all ($ t) $ list [asPredicate a,asPredicate b]


instance (AsPredicate a, AsPredicate b, AsPredicate c
         ,PredDom a ~ PredDom b, PredDom b ~ PredDom c)
         => AsPredicate (a,b,c) where
  type PredicateDomain (a,b,c) = PredDom a
  asPredicate (a,b,c) t = all ($ t) $ list [asPredicate a, asPredicate b
                                           ,asPredicate c]

instance (AsPredicate a, AsPredicate b, AsPredicate c, AsPredicate d
         ,PredDom a ~ PredDom b, PredDom b ~ PredDom c, PredDom c ~ PredDom d)
         => AsPredicate (a,b,c,d) where
  type PredicateDomain (a,b,c,d) = PredDom a
  asPredicate (a,b,c,d) t = all ($ t) $ list [asPredicate a, asPredicate b
                                             ,asPredicate c, asPredicate d]

instance (AsPredicate a, AsPredicate b, AsPredicate c, AsPredicate d
         ,AsPredicate e
         ,PredDom a ~ PredDom b, PredDom b ~ PredDom c, PredDom c ~ PredDom d
         ,PredDom d ~ PredDom e)
         => AsPredicate (a,b,c,d,e) where
  type PredicateDomain (a,b,c,d,e) = PredDom a
  asPredicate (a,b,c,d,e) t = all ($ t) $ list [asPredicate a, asPredicate b
                                               ,asPredicate c, asPredicate d
                                               ,asPredicate e]
