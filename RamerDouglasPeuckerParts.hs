module RamerDouglasPeuckerParts
    ( 
        Range,
        Stack,
        Point,
        All(..),
        findFarthestPoints,
        initStack,
        recurProcess,
        myfst,
        mysnd,
        trd,
        getRightSliceFromStack
    ) where

import Data.Maybe
import Data.List

type Range = [Int]
type Stack = [Range]
type Point = (Int, Int) 

data All = All { finalContours :: [Point], stack :: Stack } deriving Show


getFinalContours :: All -> [Point]
getFinalContours (All fc s) = fc

getStack :: All -> Stack
getStack (All fc s) = s

getRightSliceFromStack :: All -> Range
getRightSliceFromStack all = last (getStack all)

replaceStack :: All -> Stack -> All
replaceStack (All fc s) newStack = (All fc newStack)

pushSlice :: Range -> Stack -> Stack
pushSlice slice stack = slice : stack

readPt :: [Point] -> Int -> Point
readPt srcContours pos = srcContours !! pos

readDstPt :: [Point] -> Int -> Point
readDstPt dstContours pos = dstContours !! pos

writePt :: [Point] -> Point -> [Point]
writePt fc point = fc ++ [point]

findFarthestPoints :: [Point] -> Int -> Double -> Double -> Double -> Range -> Int -> Int -> Point -> (Range, Point, Bool)
findFarthestPoints srcContours iters dist max_dist epsi right_slice count pos start_pt
        | iters == 0 = (right_slice, start_pt, (max_dist <= epsi))
        | otherwise  = findFarthestPoints srcContours (iters - 1) dist (fst nestedFunc) epsi (snd nestedFunc) count cntPos (readPt srcContours cntPos)
        where
                cntPos     = cntPosFunc pos right_slice count
                nestedFunc = findFarthestPointsSecond srcContours count 1 (readPt srcContours 1) max_dist start_pt right_slice
                  
cntPosFunc :: Int -> Range -> Int -> Int             
cntPosFunc pos right_slice count = (pos + (head right_slice)) `mod` count              
                                                       
findFarthestPointsSecond :: [Point] -> Int -> Int -> Point -> Double -> Point -> Range -> (Double, Range)
findFarthestPointsSecond srcContours count j pt max_dist start_pt right_slice
        | count == j = (max_dist, right_slice)                
        | otherwise  = findFarthestPointsSecond srcContours count (j + 1) (readPt srcContours (j + 1)) (fst dist_slice) start_pt (snd dist_slice)
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

initStack :: [Point] -> Range -> Range -> Point -> Bool -> Int -> All -> All
initStack srcContours right_slice slice start_pt le_eps count (All dstC stck)
        | le_eps == True = All (writePt [] start_pt) []
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

recurProcess :: All -> [Point] -> Double -> Range -> Int -> [Point]
recurProcess (All fC []) srcContours epsi right_slice count = fC  
recurProcess all srcContours epsi right_slice count         
        |le_eps == True  = recurProcess (All (writePt (getFinalContours all) new_start_pt) crrntStck) srcContours epsi new_right_slice count
        |otherwise       = recurProcess (All (getFinalContours all) (pushSlice last_slice (pushSlice last_right_slice crrntStck))) srcContours epsi last_right_slice count
        where
                slice             = head (getStack all)
                crrntStck         = tail (getStack all) 
                end_pt            = srcContours !! (last slice)
                pos               = head slice
                start_pt          = srcContours !! pos
                recuFunc          = recursiveFunc all srcContours epsi slice right_slice start_pt end_pt pos count
                new_start_pt      = myfst recuFunc
                le_eps            = mysnd recuFunc
                new_right_slice   = trd recuFunc      
                last_right_slice  = changeSecondValue new_right_slice (last slice)
                last_slice        = changeSecondValue slice (head new_right_slice)                         

recursiveFunc :: All -> [Point] -> Double -> Range -> Range -> Point -> Point -> Int -> Int -> (Point, Bool, Range)
recursiveFunc all srcContours epsi slice right_slice start_pt end_pt pos count
        | pos /= last slice = (start_pt, le_eps, (fst range_dist))
        | otherwise         = (srcContours !! (head slice), True, right_slice)
        where 
                dx         = dxFunc end_pt start_pt
                dy         = dyFunc end_pt start_pt
                max_dist   = 0
                range_dist = secondRecursiveFunc srcContours count dx dy max_dist pos slice right_slice start_pt
                newDis     = snd range_dist 
                le_eps     = fromIntegral (newDis * newDis) <= epsi * fromIntegral (dx * dx + dy * dy)

dxFunc :: Point -> Point -> Int
dxFunc end_pt start_pt = (fst end_pt) - (fst start_pt)

dyFunc :: Point -> Point -> Int
dyFunc end_pt start_pt = (snd end_pt) - (snd start_pt)
                
secondRecursiveFunc :: [Point] -> Int -> Int -> Int -> Int -> Int -> Range -> Range -> Point -> (Range, Int) 
secondRecursiveFunc srcContours count dx dy max_dist pos slice right_slice start_pt
        | pos == (last slice)                      = (right_slice, max_dist) 
        | pos /= (last slice) && (dist > max_dist) = secondRecursiveFunc srcContours count dx dy dist (addSafePos pos count) slice new_right_slice start_pt
        | otherwise                                = secondRecursiveFunc srcContours count dx dy max_dist (addSafePos pos count) slice right_slice start_pt
        where
                new_right_slice = changeFirstValue right_slice ((pos + count - 1) `mod` count)
                pt              = readPt srcContours pos
                dist            = abs (((snd pt) - (snd start_pt)) * dx - ((fst pt) - (fst start_pt)) * dy)


addSafePos :: Int -> Int -> Int
addSafePos pos count
        | (pos + 1) >= count = 0
        | otherwise          = pos + 1



last_stage :: Int -> Double -> Int -> Int -> Int -> Point -> Int -> Point -> [Point] -> [Point]
last_stage i epsi count new_count pos start_pt wpos pt dstContours
        | i == count || new_count <= 2 = dstContours
        | otherwise                    = last_stage (l_s_f_fst !! 0) epsi count (l_s_f_fst !! 1) (addSafePos (l_s_f_fst !! 3) count) (l_s_f_trd !! 0) (l_s_f_fst !! 2) (l_s_f_trd !! 1) l_s_f_snd
        where
                end_pt                   = readDstPt dstContours pos
                dx                       = dxFunc end_pt start_pt
                dy                       = dyFunc end_pt start_pt
                dist                     = abs (((fst pt) - (fst start_pt)) * dy - ((snd pt) - (snd start_pt)) * dx)
                successive_inner_product = (((fst pt) - (fst start_pt)) * ((fst end_pt) - (fst pt)) + ((snd pt) - (snd start_pt)) * ((snd end_pt) - (snd pt)))
                l_s_f                    = last_stage_decision dist successive_inner_product epsi dx dy i count wpos pos new_count start_pt end_pt pt dstContours
                l_s_f_fst                = myfst l_s_f
                l_s_f_snd                = mysnd l_s_f
                l_s_f_trd                = trd l_s_f 


last_stage_decision :: Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Point -> Point -> Point -> [Point] -> ([Int], [Point], [Point])
last_stage_decision dist succesive_inner_product eps dx dy i count wpos pos new_count start_pt end_pt pt dstContours 
        | condition = ([(i+1), (new_count - 1), (addSafePos wpos count), (addSafePos pos count)], finalDstContours, [end_pt, (readDstPt finalDstContours pos)])
        | otherwise = ([(i+1), new_count, (addSafePos wpos count), pos], finalSecondDstContours, [pt, pt])
        where
                condition              = fromIntegral (dist * dist) <= 0.5 * eps * fromIntegral (dx * dx + dy * dy) && dx /= 0 && dy /= 0 && succesive_inner_product >= 0
                finalDstContours       = replaceListValue dstContours end_pt wpos
                finalSecondDstContours = replaceListValue dstContours pt wpos


replaceListValue :: [Point] -> Point -> Int -> [Point]
replaceListValue (x:dstContours) end_pt wpos
        | wpos == 0 = end_pt : dstContours
        | otherwise = x : replaceListValue dstContours end_pt (wpos - 1)        
















