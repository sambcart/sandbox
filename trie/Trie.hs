module Trie where

import Prelude hiding (lookup)
import Control.Monad
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char

data Trie k a = Trie
  { value  :: !(Maybe a)
  , forest :: !(Map.Map k (Trie k a))
  } deriving Eq

instance Functor (Trie k) where
  fmap f (Trie val frst) = Trie (fmap f val) (fmap (fmap f) frst)

-- The empty trie.
empty :: Trie k a
empty = Trie Nothing Map.empty

-- Returns True if the trie contains a value at its root, False otherwise.
terminal :: Trie k a -> Bool
terminal = isJust . value

-- Returns True if the trie has no sub-tries, False otherwise.
null :: Trie k a -> Bool
null = Map.null . forest

-- Constructs a trie from a list of key-value pairs.
fromList :: Ord k => [([k], a)] -> Trie k a
fromList = foldr (uncurry insert) empty

-- Constructs a list of key-value pairs from a trie.
toList :: Ord k => Trie k a -> [([k], a)]
toList t = do
  (k, t') <- Map.toList (forest t)
  msum [ do let Just a = value t'
            guard (terminal t')
            return ([k], a)
       , do (ks, a) <- toList t'
            return ((k:ks), a) ]

-- Walk down the trie a single step, and return the resulting sub-trie.
step :: Ord k => k -> Trie k a -> Maybe (Trie k a)
step k = Map.lookup k . forest

-- Walk down the trie following the key, and return the resulting sub-trie.
walk :: Ord k => [k] -> Trie k a -> Maybe (Trie k a)
walk (k:ks) t = step k t >>= walk ks
walk []     t = Just t

-- Lookup the value at a key in the trie.
lookup :: Ord k => [k] -> Trie k a -> Maybe a
lookup ks t = walk ks t >>= value

-- Returns True if there is a value at the specified key in the trie.
member :: Ord k => [k] -> Trie k a -> Bool
member ks t = isJust $ lookup ks t

-- Insert a new key and value in the trie. If the key is already present in the
-- trie, the associated value is replaced with the supplied value.
insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert key val = alter (const (Just val)) key

-- Insert with a function, combining new value and old value. The expression
-- (insertWith f k v trie) will insert the pair (k, v) if key does not exist in
-- the trie. If the key does exist, it will insert the pair (k, f new_value old_value).
insertWith :: Ord k => (a -> a -> a) -> [k] -> a -> Trie k a -> Trie k a
insertWith f key val = alter (maybe (Just val) (Just . f val)) key

-- Delete a key and its value from the trie. When the key is not a member of
-- the trie, the original trie is returned.
delete :: Ord k => [k] -> Trie k a -> Trie k a
delete key = alter (const Nothing) key

-- Update a value at a specific key with the result of the provided function.
-- When the key is not a member of the trie, the original trie is returned.
adjust :: Ord k => (a -> a) -> [k] -> Trie k a -> Trie k a
adjust f = alter (liftM f)

-- The expression (update f k trie) updates the value a at k (if it is in the
-- trie). If (f a) is Nothing, the element is deleted. If it is (Just w), the
-- key k is bound to the new value w.
update :: Ord k => (a -> Maybe a) -> [k] -> Trie k a -> Trie k a
update f = alter ((=<<) f)

-- The expression (alter f k trie) alters the value a at k, or absence thereof.
-- alter can be used to insert, delete, or update a value in a trie.
alter :: Ord k => (Maybe a -> Maybe a) -> [k] -> Trie k a -> Trie k a
alter f []     t = t { value = f (value t) }
alter f (k:ks) t = t { forest = Map.alter f' k (forest t) }
  where f' = return . alter f ks . maybe empty id

-- The expression (union t1 t2) takes the left-biased union of t1 and t2. It prefers
-- t1 when duplicate keys are encountered.
union :: Ord k => Trie k a -> Trie k a -> Trie k a
union = unionWith const

-- Union with a combining function.
unionWith :: Ord k => (a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWith f (Trie v1 f1) (Trie v2 f2) = Trie v3 f3
  where v3 = f <$> v1 <*> v2
        f3 = Map.unionWith (unionWith f) f1 f2


parseLine :: String -> Either ParseError (String, (Int, Float))
parseLine = flip parse "" $ do
  ix  <- integer ; char ','
  wrd <- word    ; char ','
  frq <- float
  return (wrd, (read ix, read frq))
  where word    = many1 letter
        integer = many1 digit
        decimal = option "" $ (:) <$> char '.' <*> integer
        float   = (++) <$> integer <*> decimal

fromFile :: FilePath -> IO (Trie Char (Int, Float))
fromFile file = do
  txt <- readFile file
  let dat = rights $ map parseLine $ lines txt
  return $ fromList dat

main :: IO ()
main = do
  trie <- fromFile "PG_2006_cs.txt"
  forever $ do
    word <- putStr "Enter word: " >> getLine
    case lookup word trie of
      Nothing  -> putStrLn $ "  " ++ word ++ " : not found!"
      Just dat -> putStrLn $ "  " ++ word ++ " : " ++ show dat
