-- | Some order theory classes
--
-- This module should really re-export everything from
-- https://hackage.haskell.org/package/lattices but this is just an experiment
-- for now, so roll our own but in a compatible way (ie, one day this whole
-- file should go away and just export Algebra.PartialOrd and Algebra.Lattice)
module Algebra.Algebra (
  -- * Partial Orders
  PartialOrd (..)
  -- * Semilattices
  , JoinSemiLattice (..)
  , joinLeq
  , BoundedJoinSemiLattice (..)
  , MeetSemiLattice (..)
  , meetLeq
  -- * Monoid wrappers
  , Join(..)
  ) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Semigroup (Semigroup(..))

-- | A partial ordering on sets is a set equipped with a binary relation 'leq' that obeys
-- the following laws
--
-- * Reflexivity: @a `leq` a@
-- * Antisymmetry: @if (a `leq` b && b `leq` a) then a == b else True@
-- * Transitivity: @if a ``leq`` b && b ``leq`` c then a ``leq`` c else True@
--
class Eq a => PartialOrd a where
  -- | @a `leq` b@ returns 'True' if @a@ is less than or equal to @b@ in the
  -- partial ordering.  Note that @not (a `leq` b)@ is not the same as @b `leq`
  -- a@ - that is only the case if the two elements are 'comparable'.  In
  -- general two arbitrary elements in a partial order need not be comparable.
  leq :: a -> a -> Bool
  -- | Two elements are comparable if @a `leq` b \\\/ b `leq` a@
  comparable :: a -> a -> Bool
  comparable a b = a `leq` b || b `leq` a
  {-# MINIMAL leq #-}
  
instance PartialOrd () where
  () `leq` () = True

-- | The pointwise ordering on pairs: @(a,b) `leq` (a',b')@ iff @a `leq` a'@ and @b `leq` b'@
instance (PartialOrd a, PartialOrd b) => PartialOrd (a,b) where
  (a,b) `leq` (a', b') = a `leq` a' && b `leq` b'

-- | The pointwise ordering on 3-tuples
instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a,b,c) where
  (a,b,c) `leq` (a',b',c') = a `leq` a' && b `leq` b' && c `leq` c'

-- | The subset ordering on sets - individual elements are not compared
instance Ord a => PartialOrd (Set.Set a) where
  leq = Set.isSubsetOf

-- | The subset ordering on the keys of the map with values ordered pointwise.
-- @Map.fromList [("a", 1)] `leq` Map.fromList [("a", 2), ("b", 3)]@ but not
-- @Map.fromList [("a", 4)] `leq` Map.fromList [("a", 2), ("b", 3)]@ and not
-- @Map.fromList [("a", 1), ("c", 5)] `leq` Map.fromList [("a", 2), ("b", 3)]@.
instance (Ord k, PartialOrd v) => PartialOrd (Map.Map k v) where
  leq = Map.isSubmapOfBy leq

-- | A join semilattice is a set equipped with a binary operation ('\/') subject to
--
-- * Idempotency: @x \\\/ x == x@
-- * Associativity: @ x \\\/ (y \\\/ z) == (x \\\/ y) \\\/ z @
-- * Commutativity: @ x \\\/ y == y \\\/ x @
class JoinSemiLattice a where
  (\/) :: a -> a -> a
  
instance JoinSemiLattice () where
  () \/ () = ()

-- | The product of two join semilattices is another join semilattice where the
-- joins are performed pointwise
instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (a, b) where
  (a,b) \/ (a', b') = (a \/ a', b \/ b')

-- | The product of three join semilattices is another join semilattice where the
-- joins are performed pointwise
instance (JoinSemiLattice a, JoinSemiLattice b, JoinSemiLattice c) => JoinSemiLattice (a, b, c) where
  (a,b,c) \/ (a', b', c') = (a \/ a', b \/ b', c \/ c')

instance Ord a => JoinSemiLattice (Set.Set a) where
  (\/) = Set.union

instance (Eq a, Hashable a) => JoinSemiLattice (HashSet.HashSet a) where
  (\/) = HashSet.union

instance (Eq k, Hashable k, JoinSemiLattice a) => JoinSemiLattice (HashMap.HashMap k a) where
  (\/) = HashMap.unionWith (\/)

-- | A meet semilattice is a set equipped with a binary operation ('/\') subject to
--
-- * Idempotency: @x \/\\ x == x@
-- * Associativity: @ x \/\\ (y \/\\ z) == (x \/\\ y) \/\\ z @
-- * Commutativity: @ x \/\\ y = y \/\\ x @
class MeetSemiLattice a where
  (/\) :: a -> a -> a

instance MeetSemiLattice () where
  () /\ () = ()

instance (MeetSemiLattice a, MeetSemiLattice b) => MeetSemiLattice (a,b) where
  (a,b) /\ (a',b') = (a /\ a', b /\ b')
  
instance (MeetSemiLattice a, MeetSemiLattice b, MeetSemiLattice c) => MeetSemiLattice (a,b,c) where
  (a,b,c) /\ (a',b',c') = (a /\ a', b /\ b', c /\ c')

instance Ord a => MeetSemiLattice (Set.Set a) where
  (/\) = Set.intersection

instance (Eq a, Hashable a) => MeetSemiLattice (HashSet.HashSet a) where
  (/\) = HashSet.intersection

instance (Eq k, Hashable k, MeetSemiLattice a) => MeetSemiLattice (HashMap.HashMap k a) where
  (/\) = HashMap.intersectionWith (/\)

-- | A bounded join semilattice has a distinguished element 'bottom' subject to
--
-- * Identity: @x \/ bottom == x@
class JoinSemiLattice a => BoundedJoinSemiLattice a where
  bottom :: a

instance BoundedJoinSemiLattice () where
  bottom = ()

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
  bottom = (bottom, bottom)

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b, BoundedJoinSemiLattice c) => BoundedJoinSemiLattice (a, b, c) where
  bottom = (bottom, bottom, bottom)

instance (Eq k, Hashable k, JoinSemiLattice a) => BoundedJoinSemiLattice (HashMap.HashMap k a) where
  bottom = HashMap.empty

-- | Monoid wrapper for 'JoinSemiLattice'
newtype Join a = Join { getJoin :: a }
  deriving (Bounded, Eq, Ord)

instance Functor Join where
  fmap f = Join . f . getJoin

instance Applicative Join where
  pure = Join
  mf <*> mx = Join (getJoin mf $ getJoin mx)

instance Monad Join where
  return = pure
  mx >>= mf = mf (getJoin mx)

instance Foldable Join where
  foldMap f = f . getJoin

instance Traversable Join where
  traverse f = fmap Join . f . getJoin

instance BoundedJoinSemiLattice a => Semigroup (Join a) where
  Join a <> Join b = Join (a \/ b)

instance BoundedJoinSemiLattice a => Monoid (Join a) where
  mempty = Join bottom
  mappend = (<>)

-- | An implementation of 'leq' induced by ('\/')
--
-- @a `leq` b = a \/ b == b@
joinLeq :: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLeq a b = a \/ b == b

-- | An implementation of 'leq' induced by ('/\')
--
-- @a `leq` b = a /\ b == a@
meetLeq :: (Eq a, MeetSemiLattice a) => a -> a -> Bool
meetLeq a b = a /\ b == a
