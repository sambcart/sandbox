module H99 where

import System.Random
import Control.Monad
import Data.List


cons, snoc :: a -> [a] -> [a]
cons   = (:)
snoc a = foldr (:) [a]


-- Problem 8, Eliminate consecutive duplicates
-- of list elements.

compressGroups :: Eq a => [[a]] -> [a]
compressGroups grps = [head g | g <- grps, length g > 0]

compress :: Eq a => [a] -> [a]
compress = compressGroups . pack


-- Problem 9, Pack consecutive duplicates of list
-- elements into sublists.

packGroups :: Eq a => [a] -> [a] -> [[a]]
packGroups g  []    = [g]
packGroups [] (a:d) = packGroups [a] d
packGroups g  (a:d)
  | a == head g = packGroups (g ++ [a]) d
  | otherwise   = g : packGroups [a] d

pack :: Eq a => [a] -> [[a]]
pack = packGroups []


-- Problem 10, Run-length encoding of a list; use
-- previous result to implement run-length encoding
-- data compression method.

encode :: Eq a => [a] -> [(Int,a)]
encode ls = zip (map length grps) vals
  where grps = pack ls
        vals = compressGroups grps


-- Problem 11, Modified run-length encoding; modify
-- previous result in such a way that if an element
-- has no duplicates it is simply copied into the
-- result list.

data Encoding a = Single a | Multiple Int a
  deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified ls = do
  (n,v) <- encode ls
  case compare n 1 of
    LT -> []
    EQ -> return $ Single v
    GT -> return $ Multiple n v


-- Problem 12, Decode a run-length encoded list; given
-- a run-length code list generated as specified in
-- P11, construct its uncompressed version.

decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified cs = cs >>= \c ->
  case c of
    Single v     -> [v]
    Multiple n v -> replicate n v


-- Problem 13, Run-length encoding of list (direct
-- solution); implement the run-length encoding data
-- compression method directly. I.e. don't explicitly
-- create the sublists containing the duplicates, only
-- count them.

runLengths :: Eq a => [a] -> [(Int,a)]
runLengths []    = []
runLengths (a:d) = counter [] 1 a d
  where counter prevs curr a []
          | curr > 0  = prevs ++ [(curr,a)]
          | otherwise = prevs
        counter prevs curr a (a':d)
          | a == a'   = counter prevs (curr + 1) a' d
          | otherwise = counter (prevs ++ [(curr,a)]) 1 a' d

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect ls = do
  (n,v) <- runLengths ls
  case compare n 1 of
    LT -> []
    EQ -> return $ Single v
    GT -> return $ Multiple n v


-- Problem 14, Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli []    = []
dupli (a:d) = a : a : dupli d


-- Problem 15, Replicate the elements of a list a
-- given number of times.

repli :: [a] -> Int -> [a]
repli []    _ = []
repli (a:d) n = replicate n a ++ repli d n


-- Problem 16, Drop every Nth element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery ls n = h ++ dropEvery t n
  where h = take (n - 1) ls
        t = drop n ls


-- Problem 17, Split a list into two parts; the
-- length of the first part is given.

split :: [a] -> Int -> ([a],[a])
split ls n = (take n ls, drop n ls)


-- Problem 18, Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice ls i j = take (j - i) $ drop i ls


-- Problem 19, Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate ls n =
  let rotL ls = snoc (head ls) (tail ls)
      rotR ls = cons (last ls) (init ls) in
  case compare n 0 of
    LT -> iterate rotR ls !! (-n)
    GT -> iterate rotL ls !! n
    EQ -> ls


-- Problem 20, Remove the Kth element from a list.

removeAt :: Int -> [a] -> Maybe (a,[a])
removeAt _ []      = Nothing
removeAt n ls
  | n < 0          = Nothing
  | n >= length ls = Nothing
removeAt 0 (a:d) = Just (a,d)
removeAt n (a:d) = do
  (b,s) <- removeAt (n - 1) d
  return (b,a:s)


-- Problem 21, Insert an element at the Kth position
-- into a list.

insertAt :: a -> [a] -> Int -> Maybe [a]
insertAt x ls n
  | n <= 0         = Just (x:ls)
  | n >= length ls = Just (ls ++ [x])
insertAt x (a:d) n = do
  r <- insertAt x d (n - 1)
  return (a:r)


-- Problem 22, Create a list containing all integers
-- within a given range.

range :: Int -> Int -> [Int]
range i j = 
  case compare i j of
    LT -> i : range (i + 1) j
    EQ -> [i]
    GT -> []


-- Problem 23, Extract a given number of randomly
-- selected elements from a list.

randSampleN :: [a] -> Int -> IO [a]
randSampleN vals num = do
  let size = length vals
      rand = randomRIO (0,size-1)
  ix <- replicateM num rand
  return [vals !! i | i <- ix]


-- Problem 24, Draw N different random numbers from
-- the set [1..M].

-- randDistinctN :: Int -> Int -> IO [Int]
-- randDistinctN n m = 



-- Problem 25, Generate a random permutation of the
-- elements of a list.

-- randPerm :: [a] -> [a]
-- randPerm ls = 



-- Problem 26, Generate the combinations of K distinct
-- objects chosen from the N elements of a list.

choose :: Eq a => Int -> [a] -> [[a]]
choose 0 ls = [[]]
choose k ls
  | k == n = [es]
  | k >  n = []
  | k <  n = 
    let p = choose (k-1) d
        q = choose k d
    in map (a:) p ++ q
  where es@(a:d) = nub ls
        n = length es


data Perm = Id | Swap Int Int | Comp Perm Perm

instance Show Perm where
  show p = case p of
    Id       -> "()"
    Swap i j -> "(" ++ show i ++ " " ++ show j ++ ")"
    Comp p q ->        show p ++ "." ++ show q

toPerm :: [(Int,Int)] -> Perm
toPerm []        = Id
toPerm [(i,j)]   = Swap i j
toPerm ((i,j):d) = Comp (Swap i j) (toPerm d)

toCycle :: [Int] -> Perm
toCycle [] = Id
toCycle ns = compose $ zipWith Swap l r
  where l = init ns
        r = tail ns

swap :: Int -> Int -> [a] -> [a]
swap i j ls = do
  ix <- [0.. length ls - 1]
  return $ case ix of
    _ | ix == i   -> ls !! j
      | ix == j   -> ls !! i
      | otherwise -> ls !! ix

compose :: [Perm] -> Perm
compose = foldr1 Comp

permute :: Perm -> [a] -> [a]
permute p ls = case p of
  Id       -> ls
  Swap i j -> swap i j ls
  Comp p q -> permute p $ permute q ls


-- Problem 27, Group the elements of a set into
-- disjoint subsets.

--   >> group [2,3] ["aldo","beat","carla","david","evi"]
--   [[["aldo","beat"],["carla","david","evi"]],
--    [["aldo","carla"],["beat","david","evi"]],
--    ...]

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint []    s = True
disjoint (a:d) s = a `notElem` s && disjoint d s

allDisjoint :: Eq a => [[a]] -> Bool
allDisjoint []     = True
allDisjoint (l:ls) = and [disjoint l s | s <- ls]

groupDisjoint :: Eq a => [Int] -> [a] -> [[[a]]]
groupDisjoint []     ls = [[]]
groupDisjoint (n:ns) ls = let ls' = nub ls in
  [ c:g | c <- choose n ls'
        , g <- groupDisjoint ns ls'
        , allDisjoint (c:g)
        ]



-- Problem 31, Determine whether a given integer is prime.

maxDiv :: Integral a => a -> a
maxDiv = truncate . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n
  | n < 0     = isPrime (-n)
  | n < 2     = False
  | n == 2    = True
  | otherwise = and [n `mod` d /= 0 | d <- [2.. maxDiv n]]


-- Problem 32, Determine the GCD of two positive integers.

myGCD :: Integral a => a -> a -> a
myGCD a b
  | a == 0 || b == 0 = undefined
  | a <  0 || b <  0 = myGCD (abs a) (abs b)
  | a /= b           = let a' = min a b in myGCD a' (max a b - a')
  | otherwise        = a


-- Problem 33, Determine whether two positive integers
-- are coprime (i.e. whether their GCD is 1).

coprime :: Integral a => a -> a -> Bool
coprime a b = myGCD a b == 1


-- Problem 34, Calculate Euler's totient function phi(n).

totient :: Integral a => a -> Int
totient n = length $ filter (coprime n) [1..n]


-- Problem 35, Determine the prime factors of a given
-- positive integer. Construct a flat list containing the
-- prime factors in ascending order.

divisors :: Integral a => a -> [a]
divisors n = [d | d <- [1.. abs n], n `mod` d == 0]

primeFactors :: Integral a => a -> [a]
primeFactors n = case divisors n of
  1:p:_ -> p : primeFactors (n `div` p)
  _     -> []


-- Problem 36, Determine the prime factors of a given
-- positive integer. Construct a list containing the prime
-- factors and their multiplicity.

primeFactorsMult :: Integral a => a -> [(a,Int)]
primeFactorsMult = map (\p -> (head p, length p)) . pack . primeFactors


-- Problem 37, Calculate Euler's totient function phi(n)
-- (improved). Let [(p1,m1), (p2,m2), ...] be the list of
-- prime factors (and their multiplicities) of n. Then
-- phi(n) can be calculated with the formula:
--    phi(n) = (p1 - 1) * p1 ** (m1 - 1) *
--             (p2 - 1) * p2 ** (m2 - 1) * ...

totient' :: Integral a => a -> a
totient' = product . map phi . primeFactorsMult
  where phi (p,m) = (p - 1) * p ^ (m - 1)


-- Problem 46/47, Define a set of logical predicates, and
-- define a function 'table' that prints the truth table of
-- a given logical expression.

data MyBool = T | F deriving (Show, Eq)

not' :: MyBool -> MyBool
not' T = F
not' F = T

and', or', nand', nor', xor', impl', equ' :: MyBool -> MyBool -> MyBool
and'  T q = q
and'  F _ = F

or'   T _ = T
or'   F q = q

nand' p q = not' $ and' p q
nor'  p q = not' $ or'  p q

xor'  T T = F
xor'  F F = F
xor'  _ _ = T

impl' T q = q
impl' F _ = T

equ'  p q = and' (impl' p q) (impl' q p)


table :: (MyBool -> MyBool -> MyBool) -> IO ()
table pred =
  let atoms = [(p,q) | p <- [T,F], q <- [T,F]]
      printLogic (p,q) = putStrLn $
        show p ++ " " ++
        show q ++ " " ++
        show (pred p q)
  in forM_ atoms printLogic

