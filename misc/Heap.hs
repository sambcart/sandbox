module Heap where

import Prelude hiding (compare)


class MinHeap h where
  size        :: h a -> Int
  depth       :: h a -> Int
  isEmpty     :: h a -> Bool
  isFull      :: h a -> Bool
  toLeft      :: h a -> Maybe (h a)
  toRight     :: h a -> Maybe (h a)
  getFirst    :: h a -> Maybe a
  getLast     :: h a -> Maybe a
  setFirst    :: h a -> a -> h a
  setLast     :: h a -> a -> h a
  heapifyDown :: Ord a => h a -> h a
  heapifyUp   :: Ord a => h a -> h a
  extract     :: Ord a => h a -> Maybe (a, h a)
  insert      :: Ord a => h a -> a -> h a

  size h
    | isEmpty h = 0
    | otherwise = 1 + lsize + rsize
      where lsize = maybe 0 size (toLeft h)
            rsize = maybe 0 size (toRight h)

  depth h
    | isEmpty h = 0
    | otherwise = 1 + max ldepth rdepth
      where ldepth = maybe 0 depth (toLeft h)
            rdepth = maybe 0 depth (toRight h)

  getLast h
    | isEmpty h = Nothing
    | otherwise = case (toLeft h, toRight h) of
      ( Nothing , Nothing ) -> getFirst h
      ( Nothing , Just r  ) -> getLast r
      ( Just l  , Nothing ) -> getLast l
      ( Just l  , Just r  )
        | depth l <= depth r -> getLast r
        | otherwise          -> getLast l

  extract h = do
    x <- getFirst h
    y <- getLast h
    let h' = setFirst h y
    return $ (x, heapifyDown h')

  -- insert h x = heapifyUp h'
  --   where h' = setLast h x


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show


instance MinHeap Tree where
  isEmpty  Leaf         = True
  isEmpty  (Node _ _ _) = False

  isFull   Leaf         = True
  isFull   (Node _ l r) = isFull l && isFull r && size l == size r

  toLeft   Leaf         = Nothing
  toLeft   (Node _ l _) = Just l

  toRight  Leaf         = Nothing
  toRight  (Node _ _ r) = Just r

  getFirst Leaf         = Nothing
  getFirst (Node x _ _) = Just x

  setFirst Leaf         y = Node y Leaf Leaf
  setFirst (Node _ l r) y = Node y l r

  setLast  Leaf         y = Node y Leaf Leaf
  setLast  (Node x l r) y =
    if not (isFull r)
    then Node x l (setLast r y)
    else Node x (setLast l y) r

-- pop :: Tree a -> Maybe (a, Tree a)
-- pop t = case t of
--   Leaf             -> Nothing
--   Node v Leaf Leaf -> Just (v, Leaf)
--   Node v l    r    -> Just (v, siftDown (Node v' l r))
--     where (v',t') = fromJust $ popLast t

-- peek :: Tree a -> Maybe a
-- peek Leaf         = Nothing
-- peek (Node v _ _) = Just v

-- popLast :: Tree a -> Maybe (a, Tree a)
-- popLast Leaf               = Nothing
-- popLast (Node v Leaf Leaf) = Just (v, Leaf)
-- popLast (Node v l r)
--   | depth l <= depth r = do { (v', r') <- popLast r; Just (v', Node v l r') }
--   | otherwise          = do { (v', l') <- popLast l; Just (v', Node v l' r) }

-- siftDown :: Tree a -> Tree a
-- siftDown t@Leaf               = t
-- siftDown t@(Node _ Leaf Leaf) = t
-- siftDown t@(Node v l r)       = if vl
--   | v <= vr = Node vr
--   where vl = peek l
--         vr = peek r



-- push :: Tree a -> a -> Tree a
-- push t a = 

-- instance Ord a => Heap (Tree a) where
--   hpush = push
--   hpop  = pop
--   hpeek = peek
