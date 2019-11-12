{-# LANGUAGE TemplateHaskell #-}

module Trie where

import Control.Lens
import Control.Monad
import qualified Data.Map as M

import Parse


data Trie k a = Trie
  { _value  :: Maybe a
  , _forest :: M.Map k (Trie k a)
  }

$( makeLenses ''Trie )

emptyTrie :: Trie k a
emptyTrie = Trie Nothing M.empty

step :: Ord k => k -> Trie k a -> Maybe (Trie k a)
step k = M.lookup k . view forest

search :: Ord k => [k] -> Trie k a -> Maybe a
search []     t = view value t
search (k:ks) t = step k t >>= search ks

insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert []     a t = set value (Just a) t
insert (k:ks) a t = over forest (M.insert k t') t
  where t' = insert ks a $ maybe emptyTrie id (step k t)

fromList :: Ord k => [([k],a)] -> Trie k a
fromList = foldr (uncurry insert) emptyTrie


parseLine :: String -> (String, (Int, Float))
parseLine = parse $ do
  ix  <- integral ; char ','
  wrd <- word     ; char ','
  frq <- floating
  return (wrd, (read ix, read frq))

fromFile :: FilePath -> IO (Trie Char (Int, Float))
fromFile path = do
  file <- readFile path
  return $ fromList $ map parseLine (lines file)

main :: IO ()
main = do
  trie <- fromFile "PG_2006_cs.txt"
  forever $ do
    word <- putStr "Enter word: " >> getLine
    case search word trie of
      Nothing  -> putStrLn $ "  " ++ word ++ " : not found!"
      Just dat -> putStrLn $ "  " ++ word ++ " : " ++ show dat
