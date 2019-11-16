{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ProjGeom where

import Data.Fixed
import Data.Function
import           Data.Set (Set)
import qualified Data.Set as Set


(~=) :: Double -> Double -> Bool
(~=) = (==) `on` round . (1e6 *)


data Point = Point Double Double
data Angle = Angle Double

-- A Line is an Angle (angular offset, modulo pi) paired
-- with a Point (the line's closest point to the origin).
-- This uniquely identifies every line representable with
-- cartesian coordinates.

data Line = Line
  { offset  :: Angle
  , closest :: Point
  } deriving Eq

instance Eq Point where
  Point a b == Point c d = a ~= c && b ~= d

instance Eq Angle where
  Angle a == Angle b = a' ~= b'
    where a' = a `mod'` pi
          b' = b `mod'` pi


data ProjectivePoint
  = PFin Point
  | PInf Angle (Set Point)
  deriving Eq

data ProjectiveLine
  = LInf
  | LFin Point Point

instance Eq ProjectiveLine where
  LInf       == LInf       = True
  LFin p1 p2 == LFin q1 q2
    | p1 == q1 && p2 == q2 = True
    | p1 == q2 && p2 == q1 = True
    | otherwise            = False






parallelTo :: Line -> Line -> Bool
parallelTo = (==) `on` offset

parallelToAll :: Line -> [Line] -> Bool
parallelToAll _ [] = True
parallelToAll l ls = and [l `parallelTo` l' | l' <- ls]

parallel :: [Line] -> Bool
parallel []     = True
parallel (l:ls) = parallelToAll l ls



-- data Point = Point Double Double
-- data Angle = Angle Double

-- data Intercept
--   = X { toCoord :: Double }
--   | Y { toCoord :: Double }

-- instance Eq Point where
--   Point x y == Point x' y' = x ~= x' && y ~= y'

-- instance Eq Angle where
--   Angle a == Angle b = a' ~= b'
--     where a' = a `mod'` pi  -- Treat angles as equal to their negatives only
--           b' = b `mod'` pi  -- for the purpose of determining line-parallelity

-- instance Eq Intercept where
--   X x == X x' = x == x'
--   Y y == Y y' = y == y'
--   X 0 == Y 0  = True
--   Y 0 == X 0  = True


-- data Line = Line
--   { toIntercept :: Intercept
--   , toAngle     :: Angle
--   } deriving Eq

-- class Linear a where
--   toLine :: a -> Line

-- instance Linear Line where
--   toLine = id

-- parallel :: Line -> Line -> Bool
-- parallel = (==) `on` toAngle


-- data Projective
--   = ProjP Point
--   | ProjI Angle Intercept Intercept


-- -- intersect :: Linear a => a -> a -> Projective
-- -- intersect (toLine -> l1) (toLine -> l2)
-- --   | parallel l1 l2 = 
-- --   | otherwise      = 


-- -- data Projective = Proj (Angle, Intercept, Intercept)
-- --   deriving Eq










