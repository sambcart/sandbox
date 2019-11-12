module Parse where

import Control.Monad
import Control.Applicative
import Data.Char


newtype Parser a = Parser { runParser :: String -> [(a,String)] }

parse :: Parser a -> String -> a
parse m s = case runParser m s of
  [(r, [])] -> r
  [(_, rs)] -> error $ "Parser error: did not consume entire stream."
  _         -> error $ "Parser error: did not understand '" ++ s ++ "'."

item :: Parser Char
item = Parser $ \s ->
  case s of
    []   -> []
    c:cs -> [(c,cs)]

failure :: Parser a
failure = Parser $ \s -> []

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> runParser p s ++ runParser q s

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case runParser p s of
    [] -> runParser q s
    rs -> rs


instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s ->
    [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure = return
  Parser cs1 <*> Parser cs2 = Parser $ \s ->
    [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Monad Parser where
  return a = Parser $ \s -> [(a,s)]
  p >>= f = Parser $ \s -> concatMap parseF (runParser p s)
    where parseF (a,s) = runParser (f a) s

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c
  then return c
  else failure

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string []     = return []
string (c:cs) = char c >> string cs >> return (c:cs)

digit :: Parser Char
digit = satisfy isDigit

integral :: Parser String
integral = do
  sgn <- string "-" <|> return []
  sig <- some digit
  return (sgn ++ sig)

floating :: Parser String
floating = do
  sgn <- string "-" <|> return []
  sig <- some digit
  man <- (char '.' >> some digit) <|> return "0"
  return (sgn ++ sig ++ "." ++ man)

word :: Parser String
word = many $ satisfy isAlphaNum <|> oneOf ".-'\""
