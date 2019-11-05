{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Language where


data SKExpr a
  = S
  | K
  | I
  | Var a
  | Ap (SKExpr a) (SKExpr a)
  deriving Eq

data MExpr a
  = MS
  | MK
  | MI
  | MVar a
  | MLam a (MExpr a)
  | MAp (MExpr a) (MExpr a)
  deriving Eq

data LExpr a
  = LVar a
  | LLam a (LExpr a)
  | LAp (LExpr a) (LExpr a)
  deriving Eq


class Show' a where
  show' :: a -> String

instance Show a => Show' a where
  show' = show

instance {-# OVERLAPPING #-} Show' String where
  show' = id

instance {-# OVERLAPPING #-} Show' Char where
  show' = (:[])

instance (Show a, Show' a) => Show (SKExpr a) where
  show t = case t of
    S      -> "S"
    K      -> "K"
    I      -> "I"
    Var a  -> show' a
    Ap t u -> "(" ++ show  t ++ " " ++ show u ++ ")"

instance (Show a, Show' a) => Show (MExpr a) where
  show t = case t of
    MS       -> "S"
    MK       -> "K"
    MI       -> "I"
    MVar a   -> show' a
    MLam a t -> "(\\" ++ show' a ++ " -> " ++ show t ++ ")"
    MAp  t u -> "("   ++ show  t ++ " "    ++ show u ++ ")"

instance (Show a, Show' a) => Show (LExpr a) where
  show t = case t of
    LVar a   -> show' a
    LLam a t -> "(\\" ++ show' a ++ " -> " ++ show t ++ ")"
    LAp  t u -> "("   ++ show  t ++ " "    ++ show u ++ ")"
