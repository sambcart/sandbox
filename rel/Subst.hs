module Subst where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M
import Lang

data Subst = Subst (Map LVar Term) Int [(Term, Term)]
  deriving (Show, Eq)

emptySubst :: Subst
emptySubst = Subst M.empty 0 []

lookup :: LVar -> Subst -> Maybe Term
lookup x (Subst m _ _) = M.lookup x m

insert :: LVar -> Term -> Subst -> Subst
insert x t (Subst m i c) = Subst (M.insert x t m) i c

extend :: Term -> Term -> Subst -> Subst
extend t u (Subst m i c) = Subst m i ((t,u):c)

walk :: LVar -> Subst -> Term
walk (LVar a) s = case lookup (LVar a) s of
  Nothing      -> Var a
  Just (Var b) -> walk (LVar b) s
  Just t       -> t

check :: Subst -> Bool
check s@(Subst _ _ c) = all (uncurry neq) c
  where Var a `neq` Var b = walk (LVar a) s /= walk (LVar b) s
        Var a `neq` u     = walk (LVar a) s /= u
        t     `neq` Var b = walk (LVar b) s /= t
        t     `neq` u     = t /= u

unify :: Term -> Term -> Subst -> Maybe Subst
unify (Var a) u s
  | u == Var a = Just s
  | otherwise  = case lookup (LVar a) s of
    Nothing -> Just $ insert (LVar a) u s
    Just t  -> unify t u s
unify t (Var a) s
  = unify (Var a) t s
unify (Suc t) (Suc u) s
  = unify t u s
unify (Cons t u) (Cons t' u') s
  = unify t t' s >>= unify u u'
unify (Pair t u) (Pair t' u') s
  = unify t t' s >>= unify u u'
unify t u s
  | t == u    = Just s
  | otherwise = Nothing

reify :: Termed a => a -> Subst -> Term
reify r s = case toTerm r of
  Var a | t == Var a -> t
        | otherwise  -> reify t s
          where t = walk (LVar a) s
  Zero     -> Zero
  Suc t    -> Suc (reify t s)
  Nil      -> Nil
  Cons t u -> Cons (reify t s) (reify u s)
  Pair t u -> Pair (reify t s) (reify u s)
