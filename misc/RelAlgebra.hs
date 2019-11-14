{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module RelAlgebra where

-- See: http://www.philipzucker.com/a-short-skinny-on-relations-towards-the-algebra-of-programming/

import qualified Data.Kind as Kind
import qualified Data.List as List
import Prelude hiding (Ordering(..), Ord(..))


type Finite a = (Bounded a, Enum a)

enumAll :: Finite a => [a]
enumAll = [minBound .. maxBound]


type Rel a b = [(a, b)]

(<~) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
r <~ s = and [x `elem` s | x <- r]

(~=) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
r ~= s = r <~ s && s <~ r


rempty :: Rel a b
rempty = []

rfull :: (Finite a, Finite b) => Rel a b
rfull = [(x, y) | x <- enumAll, y <- enumAll]

rneg :: (Eq a, Eq b, Finite a, Finite b) => Rel a b -> Rel a b
rneg f = [x | x <- rfull, x `notElem` f]


tabulate :: Finite a => (a -> b) -> Rel a b
tabulate f = [(x, f x) | x <- enumAll]

rcompose :: Eq b => Rel b c -> Rel a b -> Rel a c
rcompose xs ys = [(a, c) | (a, b) <- ys, (b1, c) <- xs, b == b1]

rid :: Finite a => Rel a a
rid = tabulate id


data PartialOrdering = LT | EQ | GT | NC
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
    | a == b = EQ
    | a <= b = LT
    | b <= a = GT
    | otherwise = NC

  a <    b = a `compare` b == LT
  a >    b = a `compare` b == GT
  a <==> b = a `compare` b /= NC
  a </=> b = a `compare` b == NC
  a <=   b = a < b || a `compare` b == EQ
  a >=   b = a > b || a `compare` b == EQ


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



infixl 8 :&&
infixl 8 :||
infixl 7 :>>

data Term = Lit Bool | (:&&) Term Term | (:||) Term Term | (:>>) Term Term
  deriving (Show, Eq)

data Ctxt = Nil | (:-) Term Ctxt
  deriving (Show, Eq)

infixr 6 :-
infixr 5 |-
infixr 5 :<=

data (|-) c t where
  Triv :: forall (c :: Ctxt).
    c |- Lit True
  Expl :: forall (c :: Ctxt) (t :: Term).
    c |- Lit False ->
    c |- t
  Var :: forall (c :: Ctxt) (t :: Term).
    t :- c |- t
  Wkn :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- u ->
    t :- c |- u
  AndIntr :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- t ->
    c |- u ->
    c |- t :&& u
  AndElim1 :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- t :&& u ->
    c |- t
  AndElim2 :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- t :&& u ->
    c |- u
  OrIntr1 :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- t ->
    c |- t :|| u
  OrIntr2 :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- u ->
    c |- t :|| u
  OrElim :: forall (c :: Ctxt) (t :: Term) (u :: Term) (v :: Term).
    c |- t :>> v ->
    c |- u :>> v ->
    c |- t :|| u ->
    c |- v
  ImplIntr :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    t :- c |- u ->
    c |- t :>> u
  ImplElim :: forall (c :: Ctxt) (t :: Term) (u :: Term).
    c |- t :>> u ->
    c |- t ->
    c |- u

data (:<=) c1 c2 where
  ReflLE :: forall (c :: Ctxt).
    c :<= c
  ConsLE :: forall (c1 :: Ctxt) (c2 :: Ctxt) (t :: Term).
    c1 :<= c2 ->
    c1 :<= t :- c2

transLE :: c1 :<= c2 -> c2 :<= c3 -> c1 :<= c3
transLE u ReflLE      = u
transLE u (ConsLE u1) = ConsLE (transLE u u1)

weakenLE :: c1 :<= c2 -> c1 |- t -> c2 |- t
weakenLE ReflLE     prf = prf
weakenLE (ConsLE u) prf = Wkn (weakenLE u prf)




data Equiv a b where
  Refl :: forall x. Equiv x x

sym :: Equiv x y -> Equiv y x
sym Refl = Refl

trans :: Equiv x y -> Equiv y z -> Equiv x z
trans Refl Refl = Refl




