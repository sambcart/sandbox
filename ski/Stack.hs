module Stack where

import Language


type Stack a = [SKExpr a]

data Redex a
  = End  (SKExpr a)
  | Step (SKExpr a, Stack a)
  deriving Eq

step :: SKExpr a -> Stack a -> Redex a
step t s = case (t, s) of
  (S, t:u:v:s)  -> Step (Ap (Ap t v) (Ap u v), s)
  (K, t:u:s)    -> Step (t, s)
  (I, t:s)      -> Step (t, s)
  (Ap t u, s)   -> Step (t, u:s)
  (S, [t,u])    -> End (Ap (Ap S t) u)
  (S, [t])      -> End (Ap S t)
  (K, [t])      -> End (Ap K t)
  (t, [])       -> End t
  _             -> error "Unmatched expr in `step`"

run :: SKExpr a -> SKExpr a
run t = go t []
  where go t s = case step t s of
          End  t      -> t
          Step (t, s) -> go t s
