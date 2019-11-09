{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Spatial where


type Coord    = Float
type Point    = (Coord, Coord)
type Bounds1D = (Coord, Coord)
type Bounds2D = (Bounds1D, Bounds1D)


class Interval t a where
  contains :: t -> a -> Bool
  midpoint :: t -> a

instance Interval Bounds1D Coord where
  contains (i, j) k = k >= i && k < j
  midpoint (i, j) = (i + j) / 2

instance Interval Bounds2D Point where
  contains (xs, ys) (x, y) = xs `contains` x && ys `contains` y
  midpoint (xs, ys) = (midpoint xs, midpoint ys)


class Region t where
  intersects :: t -> t -> Bool
  subdivide :: t -> [t]

instance Region Bounds1D where
  intersects a@(i, _) b@(j, _) = a `contains` j || b `contains` i
  subdivide a@(i, j) = [(i, m), (m, j)] where m = midpoint a

instance Region Bounds2D where
  intersects (xi, yi) (xj, yj) = xi `intersects` xj && yi `intersects` yj
  subdivide (xs, ys) = [(xi, yi) | yi <- subdivide ys, xi <- subdivide xs]


data Quadrant = NW | NE | SW | SE
  deriving (Show, Ord, Eq)

findQuadrant :: Bounds2D -> Point -> Quadrant
findQuadrant bnd (x, y) =
  if      x <= xm
  then if y <= ym then NW else SW
  else if y <= ym then NE else SE
  where (xm, ym) = midpoint bnd


data QT a
  = Leaf [(a, Point)]
  | Node
    { nw :: QTree a
    , ne :: QTree a
    , sw :: QTree a
    , se :: QTree a
    }
  deriving Show

data QTree  a = Tree
  { tree     :: QT a
  , capacity :: Int
  , region   :: Bounds2D
  }
  deriving Show


emptyLeaf :: Int -> Bounds2D -> QTree a
emptyLeaf = Tree $ Leaf []

emptyNode :: Int -> Bounds2D -> QTree a
emptyNode capacity region = Tree node capacity region
  where [nw, ne, sw, se] = map (emptyLeaf capacity) $ subdivide region
        node = Node nw ne sw se


insert :: QTree a -> (a, Point) -> QTree a
insert qt@(Tree tree capacity region) d@(obj,xy)
  | region `contains` xy =
    case tree of
      Node nw ne sw se ->
        case findQuadrant region xy of
          NW   -> qt { tree = tree { nw = insert nw d } }
          NE   -> qt { tree = tree { ne = insert ne d } }
          SW   -> qt { tree = tree { sw = insert sw d } }
          SE   -> qt { tree = tree { se = insert se d } }
      Leaf dat -> qt { tree = Leaf (d:dat) }
  | otherwise   = qt

insertAll :: QTree a -> [(a, Point)] -> QTree a
insertAll qt []     = qt
insertAll qt (d:ds) = insertAll (insert qt d) ds

buildFrom :: Int -> Bounds2D -> [(a, Point)] -> QTree a
buildFrom capacity region = insertAll (emptyLeaf capacity region)

split :: QTree a -> QTree a
split qt@(Tree (Leaf dat) capacity region)
  | length dat <= capacity = qt
  | otherwise              = split $ insertAll (emptyNode capacity region) dat
split qt@(Tree (Node nw ne sw se) _ _) =
  qt { tree = Node nw' ne' sw' se' }
  where nw' = split nw
        ne' = split ne
        sw' = split sw
        se' = split se

subsume :: QTree a -> [(a, Point)]
subsume (Tree (Leaf dat)         _ _) = dat
subsume (Tree (Node nw ne sw se) _ _) = nwD ++ neD ++ swD ++ seD
  where nwD = subsume nw
        neD = subsume ne
        swD = subsume sw
        seD = subsume se


-- query :: QTree a -> Bounds2D -> [(a, Point)]
-- query



qtree :: QTree String
qtree = buildFrom 10 box dat
  where box = ((0, 1), (0, 1))
        dat = [("a",  (0.1, 0.85)),
               ("b",  (0.2, 0.72)),
               ("c0", (0.15, 0.8)),
               ("c1", (0.15, 0.8)),
               ("c2", (0.15, 0.8)),
               ("c3", (0.15, 0.8)),
               ("c4", (0.15, 0.8)),
               ("c5", (0.15, 0.8)),
               ("c6", (0.15, 0.8)),
               ("c7", (0.15, 0.8)),
               ("c8", (0.15, 0.8)),
               ("c9", (0.15, 0.8)),
               ("d",  (0.6, 0.38)),
               ("e",  (0.4, 0.65)),
               ("f",  (0.65, 0.9))]
