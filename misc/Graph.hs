module Graph where

import Data.List


data Graph a
  = Empty
  | Node a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)
  deriving (Show, Eq)

class Graphable t where
  toGraph :: t a -> Graph a

instance Graphable [] where
  toGraph = fromNodes

instance Graphable Graph where
  toGraph = id

instance Functor Graph where
  fmap f g = case g of
    Empty       -> Empty
    Node x      -> Node (f x)
    Overlay g h -> Overlay (fmap f g) (fmap f h)
    Connect g h -> Connect (fmap f g) (fmap f h)

instance Foldable Graph where
  foldr f acc g = case g of
    Empty       -> acc
    Node x      -> f x acc
    Overlay g h -> foldr f (foldr f acc g) h
    Connect g h -> foldr f (foldr f acc g) h


fromNodes :: Foldable t => t a -> Graph a
fromNodes = foldr (Overlay . Node) Empty

fromEdge :: (a,a) -> Graph a
fromEdge (x,y) = Connect (Node x) (Node y)

fromEdges :: Foldable t => t (a,a) -> Graph a
fromEdges = foldr (Overlay . fromEdge) Empty


overlayNodes :: Foldable t => t a -> Graph a
overlayNodes = fromNodes

connectNodes :: Foldable t => t a -> Graph a
connectNodes = foldr (Connect . Node) Empty

overlay :: (Foldable t, Graphable g) => t (g a) -> Graph a
overlay = foldr (Overlay . toGraph) Empty

connect :: (Foldable t, Graphable g) => t (g a) -> Graph a
connect = foldr (Connect . toGraph) Empty


toNodes :: Graphable g => g a -> [a]
toNodes g = case toGraph g of
  Empty       -> []
  Node x      -> [x]
  Overlay g h -> toNodes g ++ toNodes h
  Connect g h -> toNodes g ++ toNodes h

toEdges :: Graphable g => g a -> [(a,a)]
toEdges g = case toGraph g of
  Empty       -> []
  Node x      -> []
  Overlay g h -> toEdges g ++ toEdges h
  Connect g h -> toEdges g ++ toEdges h ++ edges
    where edges = [(a,b) | a <- toNodes g, b <- toNodes h]


nodeElem :: (Graphable g, Eq a) => a -> g a -> Bool
nodeElem x = elem x . toNodes

edgeElem :: (Graphable g, Eq a) => (a,a) -> g a -> Bool
edgeElem (x,y) g = True

adjacent :: (Graphable g, Eq a) => a -> g a -> [a]
adjacent x g = adjL ++ adjR
  where edges = toEdges g
        adjL = [y | (y,z) <- edges, x == z]
        adjR = [z | (y,z) <- edges, x == y]

canonicalize :: (Graphable g, Ord a) => g a -> Graph a
canonicalize g = Overlay nodes edges
  where sortEdge (x,y) = if x <= y then (x,y) else (y,x)
        edges = fromEdges $ sort $ map sortEdge $ toEdges g
        nodes = fromNodes $ sort $ toNodes g

