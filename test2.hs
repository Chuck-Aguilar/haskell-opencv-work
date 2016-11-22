import Data.Maybe
import Data.List

type Range = [Int]
type Stack = [Range]
type Point = (Int, Int) 



data All = All { finalContours :: [Point], stack :: Stack } deriving Show

test = (1,2)
newCount = 0
init_iters = 3
srcContours = [(2,3),(1,2),(4,5), (1,1)]
dstContours = []

getFinalContours :: All -> [Point]
getFinalContours (All fc s) = fc

getStack :: All -> Stack
getStack (All fc s) = s

replaceStack :: All -> Stack -> All
replaceStack (All fc s) newStack = (All fc newStack)

pushSlice :: Range -> Stack -> Stack
pushSlice slice stack = slice : stack

readPt :: Int -> Point
readPt pos = srcContours !! pos

readDstPt :: Int -> Point
readDstPt pos = dstContours !! pos

writePt :: Point -> [Point]
writePt point = dstContours ++ [point]

findFarthestPoints :: Int -> Double -> Double -> Double -> Range -> Int -> Int -> Point -> (Range, Point, Bool)
findFarthestPoints iters dist max_dist epsi right_slice count pos start_pt
        | iters == 0 = (right_slice, start_pt, (max_dist <= epsi))
        | otherwise  = findFarthestPoints (iters - 1) dist (fst nestedFunc) epsi (snd nestedFunc) count cntPos (readPt cntPos)
        where
                cntPos     = cntPosFunc pos right_slice count
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

changeSecondValue :: [Int] -> Int -> [Int]
changeSecondValue xs n = changeSecondValue' xs n []

changeSecondValue' :: [Int] -> Int -> [Int] -> [Int]
changeSecondValue' [x] n acc      = acc ++ [n]
changeSecondValue' (x : xs) n acc = changeSecondValue' xs n (acc ++ [x])

initStack :: Range -> Range -> Point -> Bool -> Int -> All -> All
initStack right_slice slice start_pt le_eps count (All dstC stck)
        | le_eps == True = All (writePt start_pt) []
        | otherwise       = All [] (pushingInStack mixingFunctions)
        where
                mixingFunctions = listfy (mixTwoFunctions right_slice slice (fromJust (elemIndex start_pt srcContours)) count)

pushingInStack :: [Range] -> Stack
pushingInStack xs = foldl (\acc x -> x : acc) [] xs

listfy :: (Range, Range) -> [Range]
listfy (range1, range2) = [range1, range2]

mixTwoFunctions :: Range -> Range -> Int -> Int -> (Range, Range)
mixTwoFunctions right_slice slice pos count = (final_right_slice, final_slice)
        where
                final_right_slice = snd rightSliceMC
                final_slice       = fst rightSliceMC
                rightSliceMC      = rightSliceModCount currentRightSlice currentSlice count
                currentRightSlice = fst posMC
                currentSlice      = snd posMC
                posMC             = posModCount right_slice slice pos count

posModCount :: Range -> Range -> Int -> Int -> (Range, Range)
posModCount right_slice slice pos count = ((changeSecondValue right_slice (pos `mod` count)), (changeFirstValue slice (pos `mod` count)))

rightSliceModCount :: Range -> Range -> Int -> (Range, Range)
rightSliceModCount right_slice slice count = ((changeSecondValue slice ecuation), (changeFirstValue right_slice ecuation))
        where
                ecuation = ((head right_slice) + (head slice)) `mod` count

myfst :: (a, b, c) -> a
myfst (x, _, _) = x

mysnd :: (a, b, c) -> b
mysnd (_, x, _) = x

trd :: (a, b, c) -> c
trd (_, _, x) = x

{-recurProcess :: All -> [Point]
recurProcess (All fC []) = fC  
recurProcess all         = recurProcess recuFunc
        where
                slice     = head (getStack all)
                crrntStck = tail (getStack all) 
                end_pt    = srcContours !! (last slice)
                pos       = head slice
                start_pt  = srcContours !! pos
                recuFunc  = recursiveFunc all slice crrntStck start_pt end_pt pos-}

--rucursiveFunc :: All -> Range -> Stack -> Point -> Point -> Int -> All
recursiveFunc :: All -> Double -> Range -> Range -> Stack -> Point -> Point -> Int -> Int -> (Point, Bool, Range)
recursiveFunc all epsi slice right_slice crrntStck start_pt end_pt pos count
        | pos /= last slice = (start_pt, le_eps, (fst range_dist))
        | otherwise         = (srcContours !! (head slice), True, right_slice)
        where 
                dx         = (fst end_pt) - (fst start_pt)
                dy         = (snd end_pt) - (snd start_pt)
                max_dist   = 0
                range_dist = secondRecursiveFunc count dx dy max_dist pos slice right_slice start_pt
                newDis     = snd range_dist 
                le_eps     = fromIntegral (newDis * newDis) <= epsi * fromIntegral (dx * dx + dy * dy)
                
secondRecursiveFunc :: Int -> Int -> Int -> Int -> Int -> Range -> Range -> Point -> (Range, Int) 
secondRecursiveFunc count dx dy max_dist pos slice right_slice start_pt
        | pos == (last slice)                      = (right_slice, max_dist) 
        | pos /= (last slice) && (dist > max_dist) = secondRecursiveFunc count dx dy dist (pos + 1) slice new_right_slice start_pt
        | otherwise                                = secondRecursiveFunc count dx dy max_dist (pos + 1) slice right_slice start_pt
        where
                new_right_slice = changeFirstValue right_slice ((pos + (count - 1)) `mod` count)
                pt              = readPt pos
                dist            = abs (((snd pt) - (snd start_pt)) * dx - ((fst pt) - (fst start_pt)) * dy)
























