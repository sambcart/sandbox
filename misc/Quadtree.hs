module Quadtree where

import Data.Map (Map)
import qualified Data.Map as M

type Coord    = Float
type Point    = (Float, Float)
type Interval = (Float, Float)
type Bounds   = (Interval, Interval)

data Quadrant = NW | NE | SW | SE
  deriving (Show, Eq)

midpoint :: Interval -> Coord
midpoint (i,j) = i + (j - i) / 2

centroid :: Bounds -> Point
centroid (xs,ys) = (xm,ym)
  where xm = midpoint xs
        ym = midpoint ys

toQuadrant :: Point -> Bounds -> Quadrant
toQuadrant (x,y) bnd
  | x <= xm   = if y <= ym then NW else SW
  | otherwise = if y <= ym then NE else SE
  where (xm,ym) = centroid bnd


-- data QLeaf a = QLeaf Bounds [(a, Point)]
--   deriving Show

-- data QNode a = QNode Bounds (Map Quadrant (QTree' a))
--   deriving Show

-- data QTree' a = QL (QLeaf a) | QN (QNode a)
--   deriving Show


data QTree a = QTree Bounds [(a, Point)] (Map Quadrant (QTree a))
  deriving Show

empty :: Bounds -> QTree a
empty bounds = QTree bounds [] M.empty

updateWith :: Ord a => QTree a -> [(a, Point)] -> Int -> QTree a
updateWith tree []       _    = tree
updateWith tree (dat:ds) size = updateWith tree' ds size
  where tree' = insert dat tree size


-- insert :: Ord a => (a, Point) -> QTree a -> Int -> QTree a
-- insert dat@(a,xy) (QTree bounds dats quads)
--   Leaf bnd fringe
--     | M.size fringe < size -> Leaf bnd (M.insert a xy fringe)
--     | otherwise            -> insert dat node size
--       where node = buildNodeWith bnd (M.assocs fringe) size
--   Node bnd (nw,ne,sw,se) ->
--     case toQuadrant xy bnd of
--       NW -> Node bnd (insert dat nw size, ne, sw, se)
--       NE -> Node bnd (nw, insert dat ne size, sw, se)
--       SW -> Node bnd (nw, ne, insert dat sw size, se)
--       SE -> Node bnd (nw, ne, sw, insert dat se size)



-- data QTree a
--   = Leaf Bounds (Map a Point)
--   | Node Bounds (QTree a, QTree a, QTree a, QTree a)
--   deriving Show

-- emptyLeaf :: Bounds -> QTree a
-- emptyLeaf bnd = Leaf bnd M.empty

-- emptyNode :: Bounds -> QTree a
-- emptyNode bnd@((x0,x1),(y0,y1)) = Node bnd (nw,ne,sw,se)
--   where (xm,ym) = centroid bnd
--         nw = emptyLeaf ((x0,xm),(y0,ym))
--         ne = emptyLeaf ((xm,x1),(y0,ym))
--         sw = emptyLeaf ((x0,xm),(ym,y1))
--         se = emptyLeaf ((xm,x1),(ym,y1))

-- updateWith :: Ord a => QTree a -> [(a, Point)] -> Int -> QTree a
-- updateWith tree []       _    = tree
-- updateWith tree (dat:ds) size = updateWith tree' ds size
--   where tree' = insert dat tree size

-- buildWith :: Ord a => Bounds -> [(a, Point)] -> Int -> QTree a
-- buildWith = updateWith . emptyLeaf

-- buildNodeWith :: Ord a => Bounds -> [(a, Point)] -> Int -> QTree a
-- buildNodeWith = updateWith . emptyNode

-- insert :: Ord a => (a, Point) -> QTree a -> Int -> QTree a
-- insert dat@(a,xy) tree size = case tree of
--   Leaf bnd fringe
--     | M.size fringe < size -> Leaf bnd (M.insert a xy fringe)
--     | otherwise            -> insert dat node size
--       where node = buildNodeWith bnd (M.assocs fringe) size
--   Node bnd (nw,ne,sw,se) ->
--     case toQuadrant xy bnd of
--       NW -> Node bnd (insert dat nw size, ne, sw, se)
--       NE -> Node bnd (nw, insert dat ne size, sw, se)
--       SW -> Node bnd (nw, ne, insert dat sw size, se)
--       SE -> Node bnd (nw, ne, sw, insert dat se size)
