{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang where
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map

data LVar = LVar Int
  deriving (Show, Eq, Ord)

data Term a
  = Var Int
  | Lit a
  | Nil
  | Cons (Term a) (Term a)
  deriving Eq

class Termed a t where
  toTerm :: t -> Term a

instance Termed a (Term a) where
  toTerm = id

instance Termed a LVar where
  toTerm (LVar v) = Var v

instance Termed a t => Termed a [t] where
  toTerm []     = Nil
  toTerm (t:ts) = Cons (toTerm t) (toTerm ts)




data Ctxt a = Ctxt
  { runSubst :: Map.Map LVar (Term a)
  , runUnif  :: [(Term a, Term a)]
  , runFresh :: Int
  }


ctxtLookup :: LVar -> Ctxt a -> Maybe (Term a)
ctxtLookup lv c = Map.lookup lv (runSubst c)

ctxtInsert :: LVar -> Term a -> Ctxt a -> Ctxt a
ctxtInsert lv t c = c { runSubst = Map.insert lv t (runSubst c) }

ctxtExtend :: Term a -> Term a -> Ctxt a -> Ctxt a
ctxtExtend t1 t2 c = c { runUnif = (t1,t2) : runUnif c }

walk :: LVar -> Ctxt a -> Term a
walk lv@(LVar v) c =
  case ctxtLookup lv c of
    Nothing       -> Var v
    Just (Var v1) -> walk (LVar v1) c
    Just t        -> t

check :: Eq a => Ctxt a -> Bool
check c = all (uncurry neq) (runUnif c)
  where Var v1 `neq` Var v2 = walk (LVar v1) c /= walk (LVar v2) c
        Var v1 `neq` u      = walk (LVar v1) c /= u
        t      `neq` Var v2 = walk (LVar v2) c /= t
        t      `neq` u      = t /= u

unify :: Eq a => Term a -> Term a -> Ctxt a -> Maybe (Ctxt a)
unify (Var v) u c
  | u == Var v = Just c
  | otherwise  = case ctxtLookup (LVar v) c of
    Nothing -> Just (ctxtInsert (LVar v) u c)
    Just t  -> unify t u c
unify t (Var v) c
  = unify (Var v) t c
unify (Cons t t1) (Cons u u1) c
  = unify t u c >>= unify t1 u1
unify t u c
  = if t == u
    then Just c
    else Nothing

reify :: Eq a => Term a -> Ctxt a -> Term a
reify t c = case t of
  Var v ->
    let t1 = walk (LVar v) c
    in if t1 == Var v
      then t1
      else reify t1 c
  Lit a      -> Lit a
  Nil        -> Nil
  Cons t1 t2 -> Cons (reify t1 c) (reify t2 c)


