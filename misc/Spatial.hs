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
emptyNode capacity region = Tree (Node nw ne sw se) capacity region
  where [nw, ne, sw, se] = map (emptyLeaf capacity) $ subdivide region


insert :: QTree a -> (a, Point) -> QTree a
insert qt@(Tree tree capacity region) d@(obj,xy)
  | not (region `contains` xy) = qt
  | otherwise = case tree of
    Leaf dat         -> qt { tree = Leaf (d:dat) }
    Node nw ne sw se ->
      case findQuadrant region xy of
        NW -> qt { tree = tree { nw = insert nw d } }
        NE -> qt { tree = tree { ne = insert ne d } }
        SW -> qt { tree = tree { sw = insert sw d } }
        SE -> qt { tree = tree { se = insert se d } }

query :: QTree a -> Bounds2D -> [(a, Point)]
query qt@(Tree tree capacity region) box
  | not (region `intersects` box) = []
  | otherwise = case tree of
    Leaf dat         -> [ d | d@(obj,xy) <- dat, box `contains` xy ]
    Node nw ne sw se -> datNW ++ datNE ++ datSW ++ datSE
      where
      datNW = query nw box
      datNE = query ne box
      datSW = query sw box
      datSE = query se box

split :: QTree a -> QTree a
split qt@(Tree tree capacity region) =
  case tree of
    Leaf dat
      | length dat <= capacity -> qt
      | otherwise              -> split $ insertAll (emptyNode capacity region) dat
    Node nw ne sw se           -> qt { tree = Node nw' ne' sw' se' }
      where
      nw' = split nw
      ne' = split ne
      sw' = split sw
      se' = split se

subsume :: QTree a -> [(a, Point)]
subsume (Tree tree _ _) =
  case tree of
    Leaf dat         -> dat
    Node nw ne sw se -> datNW ++ datNE ++ datSW ++ datSE
      where
      datNW = subsume nw
      datNE = subsume ne
      datSW = subsume sw
      datSE = subsume se



insertAll :: QTree a -> [(a, Point)] -> QTree a
insertAll qt []     = qt
insertAll qt (d:ds) = insertAll (insert qt d) ds

buildFrom :: Int -> Bounds2D -> [(a, Point)] -> QTree a
buildFrom capacity region = split . insertAll (emptyLeaf capacity region)


qtree :: QTree Int
qtree = buildFrom 10 box dat
  where box = ((0, 1), (0, 1))
        dat = [(0, (0.1, 0.85)),
               (1, (0.2, 0.72)),
               (2, (0.15, 0.8)),
               (3, (0.6, 0.38)),
               (4, (0.4, 0.65)),
               (5, (0.65, 0.9))]
