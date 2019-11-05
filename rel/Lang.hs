module Lang where

data LVar = LVar Int
  deriving (Show, Eq, Ord)

data Term
  = Var Int
  | Zero
  | Suc Term
  | Nil
  | Cons Term Term
  | Pair Term Term
  deriving Eq

class Termed a where
  toTerm :: a -> Term

instance Termed LVar where
  toTerm (LVar a) = Var a

instance Termed Int where
  toTerm n | n <  0    = undefined
           | n == 0    = Zero
           | otherwise = Suc $ toTerm (n-1)

instance Termed a => Termed [a] where
  toTerm []     = Nil
  toTerm (x:xs) = Cons (toTerm x) (toTerm xs)

instance (Termed a, Termed b) => Termed (a, b) where
  toTerm (x, y) = Pair (toTerm x) (toTerm y)

instance Termed Term where
  toTerm = id

instance Show Term where
  show t = case t of
    Var a    -> "?" ++ show a
    Zero     -> "Z"
    Suc t    -> "S" ++ show t
    Nil      -> "[]"
    Cons t u -> "[" ++ showCons t u ++ "]"
      where showCons a Nil         = show a
            showCons a (Cons a' d) = show a ++ "," ++ showCons a' d
            showCons a t@(Var a')  = show a ++ "," ++ show t ++ "..."
    Pair t u -> "(" ++ show t ++ "," ++ show u ++ ")"
