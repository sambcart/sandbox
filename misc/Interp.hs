module Interp where

import Data.Map (Map)
import qualified Data.Map as M

type Var = Int

data Term
  = Zero
  | Suc Term
  | Rec Term Term Term
  | Var Var
  | Lam Var Term
  | App Term Term
  deriving (Show, Eq)

type Env = Map Var Val

data Val
  = VZero
  | VSuc Val
  | VRec Val Val Val
  | VVar Var
  | VBox Env Var Term
  | VApp Val Val
  deriving (Show, Eq)


-- isNeutral :: Val -> Bool
-- isNeutral v = case v of
--   VVar _     -> True
--   VApp v _   -> isNeutral v
--   VRec v _ _ -> isNeutral v
--   _          -> False

elimRec :: Val -> Val -> Val -> Maybe Val
elimRec tgt base step = case tgt of
  VZero  -> Just base
  VSuc v -> do
    va <- elimApp step v
    vr <- elimRec v base step
    elimApp va vr
  _      -> Just $ VRec tgt base step

elimApp :: Val -> Val -> Maybe Val
elimApp rator rand = case rator of
  VBox p x e -> reflect (M.insert x rand p) e
  _          -> Just $ VApp rator rand

reflect :: Env -> Term -> Maybe Val
reflect p e = case e of
  Zero      -> Just VZero
  Suc e     -> do
    v <- reflect p e
    return $ VSuc v
  Rec e f g -> do
    u <- reflect p e
    v <- reflect p f
    w <- reflect p g
    elimRec u v w
  Var x     -> M.lookup x p
  Lam x e   -> Just $ VBox p x e
  App e f   -> do
    u <- reflect p e
    v <- reflect p f
    elimApp u v
  
freshen :: [Var] -> Var -> Var
freshen ns n
  | n `elem` ns = freshen ns (n + 1)
  | otherwise   = n

reify :: [Var] -> Val -> Maybe Term
reify names val = case val of
  VZero      -> Just Zero
  VSuc v     -> do
    e <- reify names v
    return $ Suc e
  VRec u v w -> do
    e <- reify names u
    f <- reify names v
    g <- reify names w
    return $ Rec e f g
  VVar x     -> Just $ Var x
  VBox p x e -> do
    let y = freshen names x
    v <- reflect (M.insert x (VVar y) p) e
    f <- reify (y:names) v
    return $ Lam y f
  VApp v w   -> do
    e <- reify names v
    f <- reify names w
    return $ App e f

reflect' :: Term -> Maybe Val
reflect' = reflect M.empty

reify' :: Val -> Maybe Term
reify' = reify []

normalize :: Term -> Maybe Term
normalize e = reflect' e >>= reify'

