{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module VFA where

import Data.Map (Map)
import qualified Data.Map as M


-- type Multiset a = Eq a => a -> Int

-- empty :: Multiset a
-- empty _ = 0

-- singleton :: a -> Multiset a
-- singleton x y
--   | x == y    = 1
--   | otherwise = 0

-- insert :: a -> Multiset a -> Multiset a
-- insert x s y
--   | x == y    = s y + 1
--   | otherwise = s y

-- union :: Multiset a -> Multiset a -> Multiset a
-- union s t x = s x + t x

-- intersection :: Multiset a -> Multiset a -> Multiset a
-- intersection s t x = min (s x) (t x)


insertionSort :: Ord a => [a] -> [a]
insertionSort = insertionSortBy compare

insertionSortBy :: (a -> a -> Ordering) -> [a] -> [a]
insertionSortBy cmp = foldr insert []
  where insert x [] = [x]
        insert x (y:s) = case cmp x y of
          GT -> y : insert x s
          _  -> x : y : s


mergeSort :: Ord a => [a] -> [a]
mergeSort = mergeSortBy compare

mergeSortBy :: (a -> a -> Ordering) -> [a] -> [a]
mergeSortBy _   []  = []
mergeSortBy _   [x] = [x]
mergeSortBy cmp ls  = merge l r
  where m = length ls `div` 2
        l = mergeSortBy cmp $ take m ls
        r = mergeSortBy cmp $ drop m ls
        merge [] r = r
        merge l [] = l
        merge (x:l) (y:r) =
          case cmp x y of
            LT -> x : merge l (y:r)
            _  -> y : merge (x:l) r


toDigits :: Integral a => a -> a -> [a]
toDigits d n
  | n < 0     = undefined
  | n < d     = [n]
  | otherwise = r : toDigits d q
    where (q,r) = n `divMod` d

toDecimal :: Integral a => a -> [a]
toDecimal = toDigits 10

radixLSD :: Integral a => a -> Int -> a -> a
radixLSD base ix = go ix . toDigits base
  where go _  []     = 0
        go 0  (d:ds) = d
        go ix (_:ds) = go (ix-1) ds

radixInsert :: Integral a => a -> Int -> a -> Map a [a] -> Map a [a]
radixInsert base ix n = M.adjust (n:) (radixLSD base ix n)

radixGroup :: Integral a => a -> Int -> [a] -> Map a [a]
radixGroup base ix = foldr (radixInsert base ix) emptyBins
  where emptyBins = M.fromList $ zip [0 .. base - 1] $ repeat []

radixSort' :: Integral a => a -> [a] -> [a]
radixSort' base ns = foldl (flip step) ns rngIx
  where step ix = concatMap snd . M.toList . radixGroup base ix
        maxIx = maximum $ map (length . toDigits base) ns
        rngIx = [0 .. maxIx - 1]

radixSort :: Integral a => [a] -> [a]
radixSort = radixSort' 10


-- data Binary = B0 | B1
--   deriving (Show, Bounded, Enum, Eq, Ord)

-- data Decimal = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
--   deriving (Show, Bounded, Enum, Eq, Ord)

-- data Hexadecimal
--   = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7
--   | H8 | H9 | HA | HB | HC | HD | HE | HF
--   deriving (Show, Bounded, Enum, Eq, Ord)


-- class (Bounded r, Enum r, Ord r) => Digit r

-- class Digit r => Radix r a where
--   toDigits :: a -> [r]

-- instance Digit Binary
-- instance Digit Decimal
-- instance Digit Hexadecimal

-- instance Integral a => Radix Binary a where
--   toDigits = map (toEnum . fromIntegral) . digits 2

-- instance Integral a => Radix Decimal a where
--   toDigits = map (toEnum . fromIntegral) . digits 10

-- instance Integral a => Radix Hexadecimal a where
--   toDigits = map (toEnum . fromIntegral) . digits 16

-- -- lsd = head . toDigits
-- -- msd = head . reverse . toDigits

-- -- radixMap :: Radix r a => (a -> r) -> [a] -> Map r [a]
-- -- radixMap f = foldr (\n -> M.insertWith (++) (f n) [n]) M.empty

-- -- groupByLSD :: Radix r a => [a] -> [(r,[a])]
-- -- groupByLSD = M.toAscList . radixMap lsd

-- -- groupByMSD :: Radix r a => [a] -> [(r,[a])]
-- -- groupByMSD = M.toAscList . radixMap msd


-- -- lsd :: Radix r a => Int -> a -> [r]
-- -- lsd ndigits num = take ndigits $ toDigits num ++ repeat minBound

-- -- msd :: Radix r a => Int -> a -> [r]
-- -- msd ndigits num = zeros ++ take ndigits (reverse digits)
-- --   where digits = toDigits num
-- --         zeros = replicate (ndigits - length digits) minBound

-- -- lsd :: Radix r a => a -> Int -> r
-- -- lsd = aux . toDigits
-- --   where aux []     _ = minBound
-- --         aux (d:ds) 0 = d
-- --         aux (_:ds) i = aux ds (i-1)

