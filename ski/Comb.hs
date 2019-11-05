module Comb where

import qualified Data.List as L
import Language
import AST
import Stack


freeVars :: Eq a => MExpr a -> [a]
freeVars t = case t of
  MVar a   -> [a]
  MLam a t -> filter (a /=) (freeVars t)
  MAp  t u -> freeVars t `L.union` freeVars u
  _        -> []

free, notFree :: Eq a => a -> MExpr a -> Bool
free    a = (a `elem`)    . freeVars
notFree a = (a `notElem`) . freeVars

translate :: Eq a => MExpr a -> MExpr a
translate t = case t of
  MLam a t
    | notFree a t ->
      MAp MK (translate t)
  MLam a (MVar b)
    | a == b ->
      MI
  MLam a (MLam b t)
    | free a t ->
      let t' = translate (MLam b t) in
      translate (MLam a t')
  MLam a t@(MAp u v)
    | free a t ->
      let t1 = translate (MLam a u)
          t2 = translate (MLam a v) in
      MAp (MAp MS t1) t2
  MAp t u -> MAp (translate t) (translate u)
  t       -> t

convert :: Eq a => LExpr a -> SKExpr a
convert = m2sk . translate . l2m
  where
  l2m t = case t of
    LVar a   -> MVar a
    LLam a t -> MLam a (l2m t)
    LAp  t u -> MAp (l2m t) (l2m u)
  m2sk t = case t of
    MVar _   -> error "Encountered `MVar` in `convert` (m2sk)"
    MLam _ _ -> error "Encountered `MLam` in `convert` (m2sk)"
    MAp  t u -> Ap (m2sk t) (m2sk u)
    MS       -> S
    MK       -> K
    MI       -> I
