import Data.Maybe
import Data.List

type Range = [Int]
type Stack = [Range]
type Point = (Int, Int) 


test = (1,2)
newCount = 0
init_iters = 3
srcContours = [(1,2),(2,3),(4,5),(1,1)]
dstContours = [(4,5),(6,7),(6,7),(3,2)]

pushSlice :: Stack -> Range -> Stack
pushSlice stack slice = slice : stack

readPt :: Int -> Point
readPt pos = srcContours !! pos

readDstPt :: Int -> Point
readDstPt pos = dstContours !! pos

writePt :: Point -> [Point]
writePt point = dstContours ++ [point]

--findFarthestPoints :: Int -> Double -> Double -> Double -> Range -> Int -> Int -> Point -> (Range, Point, Bool)
--findFarthestPoints :: Int -> Double -> Double -> Double -> Range -> Int -> Int -> Point -> [Int] -> [Int]
findFarthestPoints iters dist max_dist epsi right_slice count pos start_pt acc
        | iters == 0 = (right_slice, start_pt, (max_dist <= epsi))
        -- | iters == 0 = acc
        | otherwise  = findFarthestPoints (iters - 1) dist (fst nestedFunc) epsi (snd nestedFunc) count cntPos (readPt cntPos) (cntPos : acc)
        where
                cntPos = cntPosFunc pos right_slice count
                nestedFunc = findFarthestPointsSecond count 1 (readPt 1) max_dist start_pt right_slice
                  
cntPosFunc :: Int -> Range -> Int -> Int             
cntPosFunc pos right_slice count = (pos + (head right_slice)) `mod` count              
                                                       
findFarthestPointsSecond :: Int -> Int -> Point -> Double -> Point -> Range -> (Double, Range)
findFarthestPointsSecond count j pt max_dist start_pt right_slice
        | count == j = (max_dist, right_slice)                
        | otherwise  = findFarthestPointsSecond count (j + 1) (readPt (j + 1)) (fst dist_slice) start_pt (snd dist_slice)
        where 
                dist_slice = dist_sliceFunc j pt max_dist right_slice start_pt
                                                                
dist_sliceFunc :: Int -> Point -> Double -> Range -> Point -> (Double, Range)
dist_sliceFunc j pt max_dist right_slice start_pt
        | (distFunc pt start_pt) > max_dist = ((distFunc pt start_pt), (changeFirstValue right_slice j))
        | otherwise                         = (max_dist, right_slice)

distFunc :: Point -> Point -> Double
distFunc pt start_pt = fromIntegral (dx * dx + dy * dy)
        where
                dx = (fst pt) - (fst start_pt)
                dy = (snd pt) - (snd start_pt) 


changeFirstValue :: [Int] -> Int -> [Int]
changeFirstValue (x : xs) n = n : xs

