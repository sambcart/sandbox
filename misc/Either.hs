{-# LANGUAGE FlexibleInstances #-}

module Either where
import Prelude hiding (Either(..), div)

data Either a b = Left a | Right b
  deriving Show

instance Functor (Either a) where
  fmap _ (Left a)  = Left a
  fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
  pure = Right
  Left a  <*> _       = Left a
  Right _ <*> Left  a = Left a
  Right f <*> Right b = Right (f b)

instance Monad (Either a) where
  return = pure
  Left a  >>= _ = Left a
  Right b >>= f = f b

type Error  = String
type Result = Either Error

instance {-# OVERLAPPING #-} Show a => Show (Result a) where
  show (Left err)  = err
  show (Right val) = show val

div :: Float -> Float -> Result Float
n `div` 0 = Left "Division by zero!"
n `div` d = Right (n / d)

divAll :: Float -> [Float] -> Result Float
n `divAll` []     = return n
n `divAll` (d:ds) = do
  q <- n `div` d
  q `divAll` ds
