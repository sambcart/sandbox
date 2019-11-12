{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module RelAlgebra where

-- See: http://www.philipzucker.com/a-short-skinny-on-relations-towards-the-algebra-of-programming/

import Data.Function
import Data.List


type Rel a b = [(a,b)]

type Finite a = (Bounded a, Enum a)

enumAll :: Finite a => [a]
enumAll = [minBound .. maxBound]

-- Generate explicit relation determined by bijection
tabulate :: Finite a => (a -> b) -> Rel a b
tabulate f = [(x, f x) | x <- enumAll]

-- Relation composition/identity
rcompose :: Eq b => Rel b c -> Rel a b -> Rel a c
rcompose xs ys = [(a,c) | (a,b) <- ys, (b1,c) <- xs, b == b1]

rid :: Finite a => Rel a a
rid = tabulate id

-- Arrow/category combinators
rfan :: Eq a => Rel a b -> Rel a c -> Rel a (b,c)
rfan f g = [(a,(b,c)) | (a,b) <- f, (a1,c) <- g, a == a1]

rfst :: Finite (a,b) => Rel (a,b) a
rfst = tabulate fst

rsnd :: Finite (a,b) => Rel (a,b) b
rsnd = tabulate snd

rleft :: Finite a => Rel a (Either a b)
rleft = tabulate Left

rright :: Finite b => Rel b (Either a b)
rright = tabulate Right

reither :: Eq a => Rel a c -> Rel b c -> Rel (Either a b) c
reither f g = [(Left a, c) | (a,c) <- f] ++ [(Right b, c) | (b,c) <- g]


reflectOrd :: (Ord a, Finite a) => Rel a a
reflectOrd = [(x,y) | x <- enumAll, y <- enumAll, x <= y]

tabulateSearch :: Finite a => (a -> [b]) -> Rel a b
tabulateSearch f = [(a,b) | a <- enumAll, b <- f a]

searchRel :: Eq a => Rel a b -> (a -> [b])
searchRel r a = [b | (a1,b) <- r, a == a1]

diagRel :: [a] -> Rel a a
diagRel = map (\x -> (x,x))

leftSet :: Eq a => Rel a b -> [a]
leftSet = nub . map fst

rightSet :: Eq b => Rel a b -> [b]
rightSet = nub . map snd

rSub :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
rSub r s = and [x `elem` s | x <- r]

rEq :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
rEq r s = r <~ s && s <~ r

r <~ s = rSub r s
r ~~ s = rEq r s

rdiv :: (Eq a, Eq b, Eq c, Finite a, Finite b) => Rel a c -> Rel b c -> Rel a b
rdiv r s = [(x,y) | x <- enumAll
                  , y <- enumAll
                  , let go z = (y,z) `notElem` s || (x,z) `elem` r
                  , all go (rightSet s)]


class Lattice a where
  -- Associativity, commutativity, idempotency, absorption
  join :: a -> a -> a
  meet :: a -> a -> a

class Lattice a => BoundedJoinSemiLattice a where
  -- x `join` bottom == x
  bottom :: a

class Lattice a => BoundedMeetSemiLattice a where
  -- x `meet` top == x
  top :: a

type BoundedLattice a = (BoundedJoinSemiLattice a, BoundedMeetSemiLattice a)



instance (Eq a, Eq b) => Lattice (Rel a b) where
  join r s = nub $ r ++ s
  meet r s = [x | x <- r, x `elem` s]

instance (Eq a, Eq b) => BoundedJoinSemiLattice (Rel a b) where
  bottom = []

instance (Eq a, Eq b, Finite a, Finite b) => BoundedMeetSemiLattice (Rel a b) where
  top = [(x,y) | x <- enumAll, y <- enumAll]







-- class Enum a => Countable a where
--   zero :: a
--   succ :: a -> a
--   enum :: [a]
--   enum = enumFrom zero

-- data Nat = Z | S Nat
--   deriving (Show, Eq, Ord)

-- instance Enum Nat where
--   fromEnum Z     = 0
--   fromEnum (S n) = 1 + fromEnum n
--   toEnum x = if x <= 0 then Z else S $ toEnum (x-1)

-- instance Countable Nat where
--   zero = Z
--   succ = S

-- tabulate' :: Countable a => (a -> b) -> Rel a b
-- tabulate' f = [(x, f x) | x <- enum]

-- rid' :: Countable a => Rel a a
-- rid' = tabulate' id

