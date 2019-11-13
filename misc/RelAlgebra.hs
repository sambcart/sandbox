{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module RelAlgebra where

-- See: http://www.philipzucker.com/a-short-skinny-on-relations-towards-the-algebra-of-programming/

import qualified Data.List as List
import Prelude hiding (Ordering(..), Ord(..))


type Finite a = (Bounded a, Enum a)

enumAll :: Finite a => [a]
enumAll = [minBound .. maxBound]


type Rel a b = [(a,b)]

(<~) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
r <~ s = and [x `elem` s | x <- r]

(~=) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
r ~= s = r <~ s && s <~ r


rempty :: Rel a b
rempty = []

rfull :: (Finite a, Finite b) => Rel a b
rfull = [(x,y) | x <- enumAll, y <- enumAll]

rneg :: (Eq a, Eq b, Finite a, Finite b) => Rel a b -> Rel a b
rneg f = [x | x <- rfull, x `notElem` rfull]


tabulate :: Finite a => (a -> b) -> Rel a b
tabulate f = [(x, f x) | x <- enumAll]

rcompose :: Eq b => Rel b c -> Rel a b -> Rel a c
rcompose xs ys = [(a,c) | (a,b) <- ys, (b1,c) <- xs, b == b1]

rid :: Finite a => Rel a a
rid = tabulate id


data PartialOrdering = LT' | EQ' | GT' | NC'
  deriving (Show, Read, Eq, Bounded, Enum)

class Eq a => Poset a where
  compare :: a -> a -> PartialOrdering
  (<==>)  :: a -> a -> Bool
  (</=>)  :: a -> a -> Bool
  (<)     :: a -> a -> Bool
  (<=)    :: a -> a -> Bool
  (>=)    :: a -> a -> Bool
  (>)     :: a -> a -> Bool

  a `compare` b
    | a == b = EQ'
    | a <= b = LT'
    | b <= a = GT'
    | otherwise = NC'

  a <    b = a `compare` b == LT'
  a >    b = a `compare` b == GT'
  a <==> b = a `compare` b /= NC'
  a </=> b = a `compare` b == NC'
  a <=   b = a < b || a `compare` b == EQ'
  a >=   b = a > b || a `compare` b == EQ'


class Poset a => Lattice a where
  join :: a -> a -> a
  meet :: a -> a -> a

class Lattice a => BoundedLattice a where
  top, bot :: a

class BoundedLattice a => HeytingAlgebra a where
  impl :: a -> a -> a
  impl x y = neg x `join` y

  neg :: a -> a
  neg x = x `impl` bot


instance (Eq a, Eq b) => Poset (Rel a b) where
  (<=) = (<~)

instance (Eq a, Eq b) => Lattice (Rel a b) where
  join = List.union
  meet = List.intersect

instance (Eq a, Eq b, Finite a, Finite b) => BoundedLattice (Rel a b) where
  top = rfull
  bot = rempty

instance (Eq a, Eq b, Finite a, Finite b) => HeytingAlgebra (Rel a b) where
  neg = rneg

