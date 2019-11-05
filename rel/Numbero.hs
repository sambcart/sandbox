module Numbero where

import Logic
import Lang
import Listo
import Pairo

zeroo :: Termed a => a -> Goal
zeroo o = o === Zero

nonzeroo :: Termed a => a -> Goal
nonzeroo o = fresh >>= \n -> suco n o

suco :: (Termed a, Termed b) => a -> b -> Goal
suco n o = o === Suc (toTerm n)

addo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
addo m n o =
  conde [ zeroo    m >> o === n
        , nonzeroo m >>
          do [m',o'] <- freshen 2
             suco m' m
             suco o' o
             addo m' n o'
        ]

subo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
subo m n o =
  conde [ zeroo    m >> zeroo o
        , nonzeroo m >> zeroo n >> o === m
        , do [m',n'] <- freshen 2
             suco m' m
             suco n' n
             subo m' n' o
        ]

mulo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
mulo m n o =
  conde [ zeroo m    >> zeroo o
        , nonzeroo m >>
          do [m',o'] <- freshen 2
             suco m' m
             addo o' n o
             mulo m' n o'
        ]

mulTest n = runN n $ do
  [m,n,o] <- freshen 3
  mulo m n (24 :: Int)
  pairo m n o
  return o

lengtho :: (Termed a, Termed b) => a -> b -> Goal
lengtho l o =
  conde [ nullo l >> zeroo o
        , do [l',o'] <- freshen 2
             cdro l l'
             suco o' o
             lengtho l' o'
        ]

indexo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
indexo l i o =
  conde [ zeroo i >> caro l o
        , do [i',l'] <- freshen 2
             suco i' i
             cdro l l'
             indexo l' i' o
        ]

(<?), lto :: (Termed a, Termed b) => a -> b -> Goal
lto m n =
  conde [ zeroo m >>
          do n' <- fresh
             suco n' n
        , do [m',n'] <- freshen 2
             suco m' m
             suco n' n
             lto m' n'
        ]

(<=?), leo :: (Termed a, Termed b) => a -> b -> Goal
leo m n =
  conde [ zeroo m
        , do [m',n'] <- freshen 2
             suco m' m
             suco n' n
             leo m' n'
        ]

(>?), (>=?), gto, geo :: (Termed a, Termed b) => a -> b -> Goal
gto m n = lto n m
geo m n = leo n m

(<?)  = lto
(<=?) = leo
(>?)  = gto
(>=?) = geo

rangeo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
rangeo start stop o =
  conde [ start >=? stop >> nullo o
        , start <?  stop >>
          do [next,rng] <- freshen 2
             suco start next
             conso start rng o
             rangeo next stop rng
        ]

permo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
permo n l o = do
  [r,s] <- freshen 2
  lengtho l s 
  rangeo Zero s r
  membero n r
  lengtho o n
  containso l o
  distincto o

permTest = 
  let n = 3 :: Int
      l = [1..6] :: [Int] in
  run $ do
    o <- fresh
    permo n l o
    return o


