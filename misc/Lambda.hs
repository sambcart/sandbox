module Lambda where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M


data Term v
  = Var v
  | Lam v (Term v)
  | App (Term v) (Term v)
  deriving Eq

instance Show v => Show (Term v) where
  show t = case t of
    Var x   -> showVar x
    Lam x t -> "\\" ++ showVar x ++ "." ++ show t
    App t u -> "(" ++ show t ++ " " ++ show u ++ ")"
    where showVar x = "`" ++ show x


type Env v = Map v (Val v)
data Val v = Box (Env v) v (Term v)
  deriving Show

elimApp :: Ord v => Val v -> Val v -> Maybe (Val v)
elimApp (Box p x t) v = reify (M.insert x v p) t

reify :: Ord v => Env v -> Term v -> Maybe (Val v)
reify p t = case t of
  Var x   -> M.lookup x p
  Lam x t -> Just $ Box p x t
  App t u -> do
    v <- reify p t
    w <- reify p u
    elimApp v w

vars :: Eq v => Term v -> [v]
vars t = case t of
  Var x   -> [x]
  Lam x t -> [x]    `union` vars t
  App t u -> vars t `union` vars u

free :: Eq v => Term v -> [v]
free t = case t of
  Var x   -> [x]
  Lam x t -> delete x (free t)
  App t u -> free t `union` free u

bound :: Eq v => Term v -> [v]
bound t = case t of
  Var _   -> []
  Lam x t -> [x]     `union` bound t
  App t u -> bound t `union` bound u

occurs, occursFree, occursBound :: Eq v => v -> Term v -> Bool
occurs      x = elem x . vars
occursFree  x = elem x . free
occursBound x = elem x . bound

isFresh :: Eq v => v -> Term v -> Bool
isFresh x = not . occursFree x

subst :: Eq v => v -> Term v -> Term v -> Term v
subst x s t = case t of
  Var y   | x == y    -> s
          | otherwise -> Var y
  Lam y t | x == y      -> Lam y t
          | isFresh y s -> Lam y (subst x s t)
          | otherwise   -> Lam y t
  App t u -> App (subst x s t) (subst x s u)


-- beta :: Eq v => Term v -> Term v
-- beta t = case t of
--   App (Lam x t) u -> subst x u t
--   App t u         -> App (beta t) (beta u)
--   _               -> t

-- eta :: Eq v => Term v -> Term v
-- eta t = case t of
--   Lam x (App f y) | x /= y      -> t
--                   | isFresh x f -> eta f
--                   | otherwise   -> t
--   App t u -> App (eta t) (eta u)
--   _       -> t


-- rename :: Eq v => v -> v -> Term v -> Term v
-- rename x y t = case t of
--   Var z   | x == z    -> Var y
--           | otherwise -> Var z
--   Lam z t | x == z    -> Lam y (renameLam x y t)
--           | y == z    -> Lam z t
--           | otherwise -> Lam z (rename x y t)
--   App t u             -> App (rename x y t) (rename x y u)
--   where renameLam x y (Lam z t)
--           | x == z      = Lam z t
--         renameLam x y t = rename x y t


data DeBTerm
  = DeBVar Int
  | DeBLam DeBTerm
  | DeBApp DeBTerm DeBTerm
  deriving Eq

instance Show DeBTerm where
  show = \case
    DeBVar n   -> show n
    DeBLam t   -> "\\ " ++ show t
    DeBApp t u -> "(" ++ show t ++ " " ++ show u ++ ")"


shiftDeB :: Int -> Int -> DeBTerm -> DeBTerm
shiftDeB c i t = case t of
  DeBVar n   -> DeBVar $ if n < c then n else n + i
  DeBLam t   -> DeBLam (shiftDeB (c + 1) i t)
  DeBApp t u -> DeBApp (shiftDeB c i t) (shiftDeB c i u)

substDeB :: Int -> DeBTerm -> DeBTerm -> DeBTerm
substDeB m t u = case u of
  DeBVar n   -> if m == n then t else DeBVar n
  DeBLam u   -> DeBLam $ substDeB (m + 1) (shiftDeB 0 1 t) u
  DeBApp u v -> DeBApp (substDeB m t u) (substDeB m t v)

betaDeB :: DeBTerm -> DeBTerm
betaDeB t = case t of
  DeBApp (DeBLam t) u -> shiftDeB 0 (-1) w
    where v = shiftDeB 0 1 u
          w = substDeB 0 v t
  _                   -> t
