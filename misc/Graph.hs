module Graph where

import Data.List
import Data.Function


data Graph a
  = Empty
  | Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)
  deriving (Show, Eq)


disjoint :: Eq a => [a] -> [a] -> Bool
disjoint []     _  = True
disjoint (x:xs) ys = x `notElem` ys && disjoint xs ys

intersects :: Eq a => [a] -> [a] -> Bool
intersects l r = not (disjoint l r)

subsume :: Graph a -> [a]
subsume Empty         = []
subsume (Vertex a)    = [a]
subsume (Overlay g h) = subsume g ++ subsume h
subsume (Connect g h) = subsume g ++ subsume h

looped :: Eq a => Graph a -> Bool
looped Empty         = False
looped (Vertex _)    = False
looped (Overlay g h) = looped g || looped h
looped (Connect g h)
  | subsume g `intersects` subsume h = True
  | otherwise = looped g || looped h




class Graphable t where
  toGraph :: t a -> Graph a

instance Graphable [] where
  toGraph = fromVertices

instance Graphable Graph where
  toGraph = id

instance Functor Graph where
  fmap f g = case g of
    Empty       -> Empty
    Vertex x    -> Vertex (f x)
    Overlay g h -> Overlay (fmap f g) (fmap f h)
    Connect g h -> Connect (fmap f g) (fmap f h)

instance Foldable Graph where
  foldr f acc g = case g of
    Empty       -> acc
    Vertex x    -> f x acc
    Overlay g h -> foldr f (foldr f acc g) h
    Connect g h -> foldr f (foldr f acc g) h


fromVertices :: Foldable t => t a -> Graph a
fromVertices = foldr (Overlay . Vertex) Empty

fromEdge :: (a,a) -> Graph a
fromEdge (x,y) = Connect (Vertex x) (Vertex y)

fromEdges :: Foldable t => t (a,a) -> Graph a
fromEdges = foldr (Overlay . fromEdge) Empty


overlayVertices :: Foldable t => t a -> Graph a
overlayVertices = fromVertices

connectVertices :: Foldable t => t a -> Graph a
connectVertices = foldr (Connect . Vertex) Empty

overlay :: (Foldable t, Graphable g) => t (g a) -> Graph a
overlay = foldr (Overlay . toGraph) Empty

connect :: (Foldable t, Graphable g) => t (g a) -> Graph a
connect = foldr (Connect . toGraph) Empty


toVertices :: Graphable g => g a -> [a]
toVertices g = case toGraph g of
  Empty       -> []
  Vertex x    -> [x]
  Overlay g h -> toVertices g ++ toVertices h
  Connect g h -> toVertices g ++ toVertices h

toEdges :: Graphable g => g a -> [(a,a)]
toEdges g = case toGraph g of
  Empty       -> []
  Vertex x    -> []
  Overlay g h -> toEdges g ++ toEdges h
  Connect g h -> toEdges g ++ toEdges h ++ edges
    where edges = [(a,b) | a <- toVertices g, b <- toVertices h]


vertexElem :: (Graphable g, Eq a) => a -> g a -> Bool
vertexElem x = elem x . toVertices

edgeElem :: (Graphable g, Eq a) => (a,a) -> g a -> Bool
edgeElem (x,y) g = True

adjacent :: (Graphable g, Eq a) => a -> g a -> [a]
adjacent x g = adjL ++ adjR
  where edges = toEdges g
        adjL = [y | (y,z) <- edges, x == z]
        adjR = [z | (y,z) <- edges, x == y]

canonicalize :: (Graphable g, Ord a) => g a -> Graph a
canonicalize g = Overlay verts edges
  where sortEdge (x,y) = if x <= y then (x,y) else (y,x)
        edges = fromEdges $ sort $ map sortEdge $ toEdges g
        verts = fromVertices $ sort $ toVertices g

