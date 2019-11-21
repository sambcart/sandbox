{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasse where

import Prelude hiding (Ord(..), Ordering(..))
import qualified Prelude

import Control.Monad
import Data.Sequence (Seq(..), ViewL(..), ViewR(..), viewl, viewr, (><))
import qualified Data.Sequence as S
import qualified Data.List as L


data PartialOrdering = NC | EQ | LT | GT
  deriving (Show, Eq)

totalOrdering :: PartialOrdering -> Prelude.Ordering
totalOrdering = \case
  NC -> undefined
  EQ -> Prelude.EQ
  LT -> Prelude.LT
  GT -> Prelude.GT

partialOrdering :: Prelude.Ordering -> PartialOrdering
partialOrdering = \case
  Prelude.EQ -> EQ
  Prelude.LT -> LT
  Prelude.GT -> GT


class Eq a => PartialOrd a where
  compare              :: a -> a -> PartialOrdering
  (<==>), (</=>)       :: a -> a -> Bool
  (<), (>), (<=), (>=) :: a -> a -> Bool

  compare a b
    | a == b = EQ
    | a <  b = LT
    | a >  b = GT
    | otherwise = NC
  
  a <    b = compare a b == LT
  a >    b = compare a b == GT
  a <==> b = compare a b /= NC
  a </=> b = compare a b == NC
  a <=   b = a < b || compare a b == EQ
  a >=   b = a > b || compare a b == EQ


instance (Eq a, Prelude.Ord a) => PartialOrd a where
  compare a b = partialOrdering (Prelude.compare a b)


isSubset :: Eq a => [a] -> [a] -> Bool
isSubset ls rs = and [x `elem` rs | x <- ls]


symDiff :: Eq a => [a] -> [a] -> ([a],[a])
symDiff ls rs = case L.uncons ls of
  Nothing -> ([], rs)
  Just (a,ls')
    | a `elem` rs -> let (r,l) = symDiff (L.delete a rs) ls' in (l,r)
    | otherwise   -> let (r,l) = symDiff rs ls' in (a:l,r)


instance {-# OVERLAPPING #-} Eq a => PartialOrd [a] where
  compare ls rs = case symDiff ls rs of
    ( [] , [] ) -> EQ
    ( [] , _  ) -> LT
    ( _  , [] ) -> GT
    _           -> NC


type Rel a = [(a,a)]

poset :: PartialOrd a => [a] -> Rel a
poset []     = []
poset (x:xs) = l ++ r ++ poset xs
  where l = [(x,y) | y <- xs, x < y]
        r = [(y,x) | y <- xs, y < x]

reflexive_completion :: Eq a => Rel a -> Rel a
reflexive_completion rel = L.nub $ rel ++ refl
  where elts = concat [[x,y] | (x,y) <- rel]
        refl = [(x,x) | x <- elts]

transitive_step :: Eq a => Rel a -> Rel a
transitive_step rel = L.nub $ rel ++ trans
  where trans = concat [[(x,z) | (y1,z) <- rel, y == y1] | (x,y) <- rel]

transitive_completion :: Eq a => Rel a -> Rel a
transitive_completion rel
  | before == after = rel
  | otherwise = transitive_step rel'
  where before = length rel
        after = length rel'
        rel' = transitive_step rel

-- initChains :: Rel a -> [[a]]
-- initChains rel = [[x,y] | (x,y) <- rel]

-- chain1 :: Eq a => [a] -> [[a]] -> [[a]]
-- chain1 ch chs = 
--   let go _  [] = []
--       go ch (ch1:chs)
--         | last ch == head ch1 = (init ch ++ ch1) : go ch chs
--         | head ch == last ch1 = (init ch1 ++ ch) : go ch chs
--         | otherwise           = go ch chs
--   in case go ch chs of
--     [] -> [ch]
--     chs1 -> concatMap (flip chain1 chs) chs1

-- buildChains :: Eq a => Rel a -> [[a]]
-- buildChains rel = chains [[x,y] | (x,y) <- rel]
--   where chains chs = L.nub $ concatMap (flip chain1 chs) chs


initChains :: Rel a -> [Seq a]
initChains rel = [S.fromList [x,y] | (x,y) <- rel]

chain1 :: Eq a => Seq a -> [Seq a] -> [Seq a]
chain1 ch chs =
  case go ch chs of
    []   -> [ch]
    chs1 -> concatMap (flip chain1 chs) chs1
  where go _   [] = []
        go ch1 (ch2:chs)
          | b1 == a2 = (init1 >< ch2) : go ch1 chs
          | a1 == b2 = (init2 >< ch1) : go ch1 chs
          | otherwise = go ch1 chs
          where a1 :< _ = viewl ch1
                a2 :< _ = viewl ch2
                init1 :> b1 = viewr ch1
                init2 :> b2 = viewr ch2

chains :: Eq a => [Seq a] -> [Seq a]
chains chs = L.nub $ concatMap (flip chain1 chs) chs

buildChains :: Eq a => Rel a -> [Seq a]
buildChains rel = chains [S.fromList [x,y] | (x,y) <- rel]


-- pprint :: (Foldable t, Show a) => t a -> IO ()
-- pprint = pprint_with pprint_seq
-- pprint = pprint_with (putStr . (++ " ") . show)
-- pprint s = forM_ s pprint_elt >> putStrLn ""
--   where pprint_elt = putStr . (++ " ") . show

pprint_seq :: (Foldable t, Show a) => t a -> IO ()
pprint_seq ls = forM_ ls (putStr . (++ " ") . show) >> putStrLn ""

pprint_with :: (Foldable t, Show a) => (a -> IO ()) -> t a -> IO ()
pprint_with prnt ls = forM_ ls prnt >> putStrLn ""


