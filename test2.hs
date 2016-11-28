import Data.Maybe
import Data.List

type Range = [Int]
type Stack = [Range]
type Point = (Int, Int) 



data All = All { finalContours :: [Point], stack :: Stack } deriving Show

test = (1,2)
newCount = 0
init_iters = 3
srcContours = [(573,95),(574,94),(580,94),(581,95),(584,95),(585,94),(587,94),(588,95),(590,95),(591,94),(611,94),(612,95),(614,95),(615,94),(616,94),(617,95),(622,95),(623,94),(624,95),(630,95),(632,97),(632,106),(631,107),(631,118),(630,119),(630,140),(631,141),(631,146),(630,147),(631,148),(631,149),(630,150),(630,160),(631,161),(631,166),(630,167),(630,169),(631,170),(630,171),(630,217),(629,218),(629,228),(630,229),(630,231),(629,232),(629,248),(628,249),(628,253),(629,254),(629,255),(630,256),(629,257),(629,260),(628,261),(629,262),(628,263),(629,264),(629,272),(628,273),(628,275),(629,276),(628,277),(629,278),(628,279),(628,283),(629,284),(628,285),(628,316),(629,317),(629,327),(628,328),(628,329),(629,330),(629,344),(630,345),(630,347),(629,348),(629,358),(630,359),(630,375),(631,376),(631,377),(630,378),(630,392),(631,393),(631,408),(629,410),(484,410),(483,409),(482,409),(481,410),(478,410),(477,409),(476,410),(475,410),(474,409),(471,409),(470,410),(464,410),(463,409),(460,409),(459,410),(458,410),(457,409),(454,409),(453,410),(438,410),(437,409),(391,409),(390,410),(380,410),(379,409),(377,409),(376,410),(277,410),(276,411),(275,410),(271,410),(270,411),(262,411),(261,410),(259,410),(258,411),(252,411),(251,410),(250,410),(249,411),(248,410),(247,411),(182,411),(181,412),(179,412),(178,411),(176,411),(175,412),(128,412),(127,413),(126,413),(125,412),(123,412),(122,413),(110,413),(109,412),(108,413),(107,413),(106,412),(101,412),(100,413),(88,413),(87,412),(84,412),(83,413),(82,413),(81,412),(80,413),(77,413),(76,412),(74,412),(73,413),(72,412),(71,413),(68,413),(67,412),(66,413),(59,413),(58,412),(48,412),(47,411),(46,412),(45,411),(34,411),(32,409),(32,407),(33,406),(33,405),(32,404),(32,399),(31,398),(31,397),(32,396),(32,393),(31,392),(31,365),(32,364),(32,362),(31,361),(32,360),(32,354),(31,353),(31,352),(32,351),(32,339),(31,338),(31,332),(32,331),(32,327),(31,326),(32,325),(32,309),(33,308),(32,307),(32,304),(33,303),(33,282),(32,281),(32,271),(33,270),(33,267),(32,266),(32,262),(31,261),(31,260),(30,260),(29,259),(28,259),(27,258),(28,257),(29,257),(30,256),(30,254),(32,252),(32,231),(31,230),(31,224),(32,223),(32,222),(31,221),(31,210),(30,209),(30,206),(31,205),(31,195),(32,194),(32,178),(31,177),(31,173),(32,172),(31,171),(31,166),(30,165),(31,164),(31,160),(30,159),(30,150),(29,149),(29,137),(28,136),(28,129),(27,128),(27,115),(26,114),(26,113),(27,112),(27,109),(26,108),(26,98),(28,96),(39,96),(40,97),(45,97),(46,96),(47,97),(52,97),(53,96),(56,96),(57,97),(61,97),(62,96),(63,96),(64,97),(65,96),(68,96),(69,97),(70,96),(71,97),(72,96),(73,96),(74,97),(75,96),(77,96),(78,97),(79,97),(80,96),(87,96),(88,97),(90,97),(91,96),(103,96),(104,97),(105,97),(106,96),(107,96),(108,97),(112,97),(113,96),(119,96),(120,97),(126,97),(127,96),(137,96),(138,97),(139,96),(143,96),(144,97),(145,96),(165,96),(166,97),(179,97),(180,96),(181,97),(182,97),(183,96),(184,97),(195,97),(196,96),(197,97),(198,96),(205,96),(206,97),(207,97),(208,96),(215,96),(216,97),(217,96),(220,96),(221,97),(227,97),(228,96),(261,96),(262,97),(263,96),(275,96),(276,97),(279,97),(280,96),(282,96),(283,97),(285,97),(286,96),(290,96),(291,97),(292,96),(296,96),(297,97),(298,96),(304,96),(305,97),(306,97),(307,96),(322,96),(323,97),(325,97),(326,96),(328,96),(329,97),(330,96),(338,96),(339,97),(347,97),(348,96),(361,96),(362,97),(366,97),(367,96),(394,96),(395,95),(447,95),(448,96),(449,95),(465,95),(466,96),(468,96),(469,95),(472,95),(473,96),(478,96),(479,95),(480,96),(481,95),(482,95),(483,96),(486,96),(487,95)]

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

readDstPt :: [Point] -> Int -> Point
readDstPt dstContours pos = dstContours !! pos

writePt :: [Point] -> Point -> [Point]
writePt fc point = fc ++ [point]

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

recurProcess :: All -> Double -> Range -> Int -> [Point]
recurProcess (All fC []) epsi right_slice count = fC  
recurProcess all epsi right_slice count         
        |le_eps == True  = recurProcess (All (writePt (getFinalContours all) new_start_pt) crrntStck) epsi new_right_slice count
        |otherwise       = recurProcess (All (getFinalContours all) (pushSlice last_slice (pushSlice last_right_slice crrntStck))) epsi last_right_slice count
        where
                slice             = head (getStack all)
                crrntStck         = tail (getStack all) 
                end_pt            = srcContours !! (last slice)
                pos               = head slice
                start_pt          = srcContours !! pos
                recuFunc          = recursiveFunc all epsi slice right_slice start_pt end_pt pos count
                new_start_pt      = myfst recuFunc
                le_eps            = mysnd recuFunc
                new_right_slice   = trd recuFunc      
                last_right_slice  = changeSecondValue new_right_slice (last slice)
                last_slice        = changeSecondValue slice (head new_right_slice)                         

recursiveFunc :: All -> Double -> Range -> Range -> Point -> Point -> Int -> Int -> (Point, Bool, Range)
recursiveFunc all epsi slice right_slice start_pt end_pt pos count
        | pos /= last slice = (start_pt, le_eps, (fst range_dist))
        | otherwise         = (srcContours !! (head slice), True, right_slice)
        where 
                dx         = dxFunc end_pt start_pt
                dy         = dyFunc end_pt start_pt
                max_dist   = 0
                range_dist = secondRecursiveFunc count dx dy max_dist pos slice right_slice start_pt
                newDis     = snd range_dist 
                le_eps     = fromIntegral (newDis * newDis) <= epsi * fromIntegral (dx * dx + dy * dy)

dxFunc :: Point -> Point -> Int
dxFunc end_pt start_pt = (fst end_pt) - (fst start_pt)

dyFunc :: Point -> Point -> Int
dyFunc end_pt start_pt = (snd end_pt) - (snd start_pt)
                
secondRecursiveFunc :: Int -> Int -> Int -> Int -> Int -> Range -> Range -> Point -> (Range, Int) 
secondRecursiveFunc count dx dy max_dist pos slice right_slice start_pt
        | pos == (last slice)                      = (right_slice, max_dist) 
        | pos /= (last slice) && (dist > max_dist) = secondRecursiveFunc count dx dy dist (addSafePos pos count) slice new_right_slice start_pt
        | otherwise                                = secondRecursiveFunc count dx dy max_dist (addSafePos pos count) slice right_slice start_pt
        where
                new_right_slice = changeFirstValue right_slice ((pos + count - 1) `mod` count)
                pt              = readPt pos
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
















