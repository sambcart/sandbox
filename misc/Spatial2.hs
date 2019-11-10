{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Spatial2 where

import Prelude hiding (getContents)
import Control.Lens hiding (contains)
import Data.Maybe
import qualified Data.Map as M

type Coord    = Float
type Point    = (Coord, Coord)
type Interval = (Coord, Coord)
type Boundary = (Interval, Interval)


{-

class Foldable t => Spatial t a where
  point :: a -> t Coord

class Spatial t a => Ranged r t a where
  midpoint :: r -> t Coord
  contains :: r -> a -> Bool

class Bounded r where
  bounds    :: r -> Boundary
  subdivide :: r -> [Boundary]

intersects :: (Bounded r, Bounded s) => r -> s -> Bool

boundaryContains :: Bounded r => r -> Point -> Bool
boundaryContains = contains . toBounds

boundaryIntersects :: Bounded r => r -> Boundary -> Bool
boundaryIntersects = intersects . toBounds

-}


class Range r a where
  midpoint    :: r -> a
  contains    :: r -> a -> Bool
  notContains :: r -> a -> Bool
  notContains r = not . contains r

instance Range Interval Coord where
  (a,b) `contains` x = x >= a && x < b
  midpoint (a,b) = (a + b) / 2

instance Range Boundary Point where
  (ab,cd) `contains` (x,y) = ab `contains` x && cd `contains` y
  midpoint (ab,cd) = (midpoint ab, midpoint cd)


class Region r where
  intersects    :: r -> r -> Bool
  notIntersects :: r -> r -> Bool
  notIntersects r = not . intersects r

instance Region Interval where
  ab `intersects` cd = ab `contains` fst cd || cd `contains` fst ab

instance Region Boundary where
  (ab,cd) `intersects` (xy,wz) = ab `intersects` xy && cd `intersects` wz


data Quadrant = NW | NE | SW | SE
  deriving (Show, Ord, Eq)

getQuadrant :: Boundary -> Point -> Quadrant
getQuadrant (xi,yi) (x,y)
  | x <= xm   = if y <= ym then NW else SW
  | otherwise = if y <= ym then NE else SE
  where (xm,ym) = midpoint (xi,yi)

subdivide :: Boundary -> Quadrant -> Boundary
subdivide bnd@((x0,x1),(y0,y1)) =
  let (xm,ym) = midpoint bnd in
  \case NW -> ((x0,xm),(y0,ym))
        NE -> ((xm,x1),(y0,ym))
        SW -> ((x0,xm),(ym,y1))
        SE -> ((xm,x1),(ym,y1))


type QMap a = M.Map Quadrant (QTree a)

data QT a = QT
  { _subtrees :: QMap a
  , _contents :: [(a, Point)]
  }
  deriving Show

data QTree a = Tree
  { _tree       :: QT a
  , getCapacity :: Int
  , getBoundary :: Boundary
  }
  deriving Show

$( makeLenses ''QT )
$( makeLenses ''QTree )


instance Range (QTree a) Point where
  contains = contains . getBoundary
  midpoint = midpoint . getBoundary

-- instance Bounded (QTree a) where
--   toBounds = getBoundary



emptyTree :: Int -> Boundary -> QTree a
emptyTree = Tree $ QT M.empty []

checkSize :: QTree a -> Bool
checkSize qtree = getSize qtree < getCapacity qtree

insertItem :: (a, Point) -> QTree a -> QTree a
insertItem = over tree . over contents . (:)

insertSubtree :: Quadrant -> QTree a -> QTree a -> QTree a
insertSubtree quadrant = over tree . over subtrees . M.insert quadrant

newSubtree :: Quadrant -> QTree a -> QTree a
newSubtree quadrant (Tree _ capacity boundary)
  = emptyTree capacity (subdivide boundary quadrant)

checkMakeSubtree :: Quadrant -> QTree a -> QTree a
checkMakeSubtree quadrant qtree =
  case getSubtree quadrant qtree of
    Nothing -> newSubtree quadrant qtree
    Just tr -> tr

checkInsert :: (a, Point) -> QTree a -> QTree a
checkInsert (a,xy) qtree@(Tree tree capacity boundary)
  | qtree `notContains` xy = qtree
  | checkSize qtree        = insertItem (a,xy) qtree
  | otherwise              = insertSubtree quadrant subtree qtree
    where quadrant = getQuadrant boundary xy
          subtree  = checkMakeSubtree quadrant qtree
          newtree  = checkInsert (a,xy) subtree


tr0, tr1, tr2, tr3 :: QTree Int
tr0 = emptyTree 2 ((0,1),(0,1))
tr1 = checkInsert (0, (0.3, 0.2)) tr0
tr2 = checkInsert (1, (0.4, 0.1)) tr1
tr3 = checkInsert (2, (0.1, 0.3)) tr2


getSubtrees :: QTree a -> QMap a
getSubtrees (Tree tree _ _) = view subtrees $ tree

getContents :: QTree a -> [(a, Point)]
getContents (Tree tree _ _) = view contents $ tree

getSize :: QTree a -> Int
getSize = length . getContents

getSubtree :: Quadrant -> QTree a -> Maybe (QTree a)
getSubtree quad qtree = M.lookup quad $ getSubtrees qtree

getSubcontents :: Quadrant -> QTree a -> Maybe [(a, Point)]
getSubcontents quad qtree = getSubtree quad qtree >>= return . getContents

getNW, getNE, getSW, getSE :: QTree a -> Maybe (QTree a)
getNW = getSubtree NW
getNE = getSubtree NE
getSW = getSubtree SW
getSE = getSubtree SE

getNWContents, getNEContents, getSWContents, getSEContents :: QTree a -> Maybe [(a, Point)]
getNWContents = getSubcontents NW
getNEContents = getSubcontents NE
getSWContents = getSubcontents SW
getSEContents = getSubcontents SE



