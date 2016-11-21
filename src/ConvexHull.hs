module ConvexHull
    ( 
    	convexHull
    ) where

import Data.List (sort)
import GHC.Int (Int32)

-- Coordinate type
type R = Int32

-- Vector / point type
type R2 = (R, R)

-- Checks if it's shortest to rotate from the OA to the OB vector in a clockwise
-- direction.
clockwise :: R2 -> R2 -> R2 -> Bool
clockwise o a b = (a `sub` o) `cross` (b `sub` o) <= 0

-- 2D cross product.
cross :: R2 -> R2 -> R
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- Subtract two vectors.
sub :: R2 -> R2 -> R2
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Implements the monotone chain algorithm
convexHull :: [R2] -> [R2]
convexHull [] = []
convexHull [p] = [p]
convexHull points = lower ++ upper
  where
    sorted = sort points
    lower = chain sorted
    upper = chain (reverse sorted)

chain :: [R2] -> [R2]
chain = go []
  where
    -- The first parameter accumulates a monotone chain where the most recently
    -- added element is at the front of the list.
    go :: [R2] -> [R2] -> [R2]
    go acc@(r1:r2:rs) (x:xs) =
      if clockwise r2 r1 x
        -- Made a clockwise turn - remove the most recent part of the chain.
        then go (r2:rs) (x:xs)
        -- Made a counter-clockwise turn - append to the chain.
        else go (x:acc) xs
    -- If there's only one point in the chain, just add the next visited point.
    go acc (x:xs) = go (x:acc) xs
    -- No more points to consume - finished!  Note: the reverse here causes the
    -- result to be consistent with the other examples (a ccw hull), but
    -- removing that and using (upper ++ lower) above will make it cw.
    go acc [] = reverse $ tail acc