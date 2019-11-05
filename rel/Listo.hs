module Listo where

import Control.Applicative
import Logic
import Lang

nullo :: Termed a => a -> Goal
nullo o = o === Nil

notNullo :: Termed a => a -> Goal
notNullo o = fresh >>= \a -> caro o a

conso :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
conso a d o = o === Cons (toTerm a) (toTerm d)

caro :: (Termed a, Termed b) => a -> b -> Goal
caro l o = fresh >>= \d -> conso o d l

cdro :: (Termed a, Termed b) => a -> b -> Goal
cdro l o = fresh >>= \a -> conso a o l

listo :: Termed a => a -> Goal
listo o =
  nullo o <|>
  do [a,d] <- freshen 2
     conso a d o

inito :: (Termed a, Termed b) => a -> b -> Goal
inito l o = do
  [a,d] <- freshen 2
  conso a d l
  conde [ nullo d >> nullo o
        , do o' <- fresh
             conso a o' o
             inito d o'
        ]

lasto :: (Termed a, Termed b) => a -> b -> Goal
lasto l o = do
  [a,d] <- freshen 2
  conso a d l
  conde [ nullo    d >> o === a
        , notNullo d >> lasto d o
        ]

snoco :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
snoco d a o =
  inito o d >> lasto o a

mapo :: (Termed a, Termed b, Termed c) => (Term -> a) -> b -> c -> Goal
mapo f l o =
  conde [ nullo l >> nullo o
        , do [a,d,a',d'] <- freshen 4
             a' === f a
             conso a d l
             conso a' d' o
             mapo f d d'
        ]

appendo :: (Termed a, Termed b, Termed c) => a -> b -> c -> Goal
appendo l r o =
  conde [ nullo l >> r === o
        , do [a,d,s] <- freshen 3
             conso a d l
             conso a s o
             appendo d r s
        ]

reverseo :: (Termed a, Termed b) => a -> b -> Goal
reverseo l o =
  conde [ nullo l >> nullo o
        , do [a,d,s,r] <- freshen 4
             conso a d l
             conso a Nil s
             appendo r s o
             reverseo d r
        ]

membero :: (Termed a, Termed b) => a -> b -> Goal
membero x l = do
  [a,d] <- freshen 2
  conso a d l
  conde [ x === a
        , membero x d
        ]

absento :: (Termed a, Termed b) => a -> b -> Goal
absento x l =
  conde [ nullo l
        , do [a,d] <- freshen 2
             x =/= a
             conso a d l
             absento x d
        ]

disjointo :: (Termed a, Termed b) => a -> b -> Goal
disjointo l s = do
  conde [ nullo l <|> nullo s
        , do [a,d] <- freshen 2
             conso a d l
             absento a s
             disjointo d s
        ]

distincto :: Termed a => a -> Goal
distincto l =
  conde [ nullo l
        , do [a,d] <- freshen 2
             conso a d l
             absento a d
             distincto d
        ]

containso :: (Termed a, Termed b) => a -> b -> Goal
containso l o =
  conde [ nullo o
        , do [a,d] <- freshen 2
             conso a d o
             membero a l
             containso l d
        ]

elemofo :: (Termed a, Termed b) => a -> [b] -> Goal
elemofo x l = conde [ x === y | y <- l ]
