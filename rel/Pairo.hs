module Pairo where

import Logic
import Lang
import Listo

pairo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
pairo a d o = o === Pair (toTerm a) (toTerm d)

fsto :: (Termed a, Termed b) => a -> b -> Goal
fsto p o = fresh >>= \d -> pairo o d p

sndo :: (Termed a, Termed b) => a -> b -> Goal
sndo p o = fresh >>= \a -> pairo a o p

zipo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
zipo l r o = do
  conde [ nullo l >> nullo o
        , nullo r >> nullo o
        , do [x,xs,y,ys,a,t] <- freshen 6
             pairo x y a
             conso x xs l
             conso y ys r
             conso a t o
             zipo xs ys t
        ]

unzipo :: (Termed a, Termed b) => a -> b -> Goal
unzipo z o = do
  conde [ nullo z >>
          pairo Nil Nil o
        , do [a,d,x,y,l,r,t] <- freshen 7
             pairo x y a
             pairo l r t
             pairo (Cons x l) (Cons y r) o
             conso a d z
             unzipo d t
        ]
