module AST where

import Language


data Prim a = PS | PK | PI | PVar a
  deriving (Show, Eq)

data AST a
  = Leaf (Prim a)
  | Node (AST a) (AST a)
  deriving (Show, Eq)

parseAST :: SKExpr a -> AST a
parseAST t = case t of
  S      -> Leaf PS
  K      -> Leaf PK
  I      -> Leaf PI
  Var a  -> Leaf (PVar a)
  Ap t u -> Node (parseAST t) (parseAST u)

evalAST :: Eq a => AST a -> AST a
evalAST t = case t of
  Leaf _ -> t
  Node (Leaf PI) x
    -> evalAST x
  Node (Node (Leaf PK) x) y
    -> evalAST x
  Node (Node (Node (Leaf PS) x) y) z
    -> evalAST (Node (Node x z) (Node y z))
  Node x y ->
    let l = evalAST x
        r = evalAST y in
    case () of
      _ | x /= l    -> evalAST (Node l y)
        | y /= r    -> evalAST (Node x r)
        | otherwise -> Node l r
