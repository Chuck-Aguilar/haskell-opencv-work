module RightReceipt
    ( 
    	findReceipt
    ) where

import ConvexHull
import ApproxPolyDP
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import GHC.Int (Int32)
import Linear.V2
import Data.Map
import Data.Maybe
import Data.List

findPerimeter :: Map Double [Point] -> [Double]
findPerimeter areaPoints = Data.Map.foldl (\acc x -> acc ++ [getPerimeter x]) [] areaPoints


getPerimeter :: [Point] -> Double
getPerimeter (firstPoint : points) = getPerimeter' points firstPoint firstPoint 0
	where
		getPerimeter' :: [Point] -> Point -> Point -> Double -> Double
		getPerimeter' [] (crrntX, crrntY) (prevX, prevY) acc = acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2)))
		getPerimeter' ((crrntX, crrntY) : point) firstPoint (prevX, prevY) acc = getPerimeter' point firstPoint (crrntX, crrntY) (acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2))))


findReceipt :: [(Double, [Point])] -> [Point]
findReceipt areaPoints = ordered
	where		
		mapAreaPoints = fromList areaPoints
		epsilons      = findEpsilon (reverse (findPerimeter mapAreaPoints))		
		allContours   = findAllPolyDp ((getListOfPoints mapAreaPoints)) epsilons
		unordered     = getReceipt allContours
		ordered		  = orderPoints unordered


getReceipt :: [[Point]] -> [Point]
getReceipt [] = []
getReceipt (x : allContours)
	| (length x) == 4 = x
	| otherwise       = getReceipt allContours


findEpsilon :: [Double] -> [Double]
findEpsilon xs = Data.List.map (* 0.02) xs


findAllPolyDp :: [[Point]] -> [Double] -> [[Point]]
findAllPolyDp listOfPoints epsilons = Data.List.map (uncurry minifyPolygon) (zip listOfPoints epsilons)


getListOfPoints :: Map Double [Point] -> [[Point]]
getListOfPoints areaPoints = Data.Map.foldl (\acc x -> x : acc) [] areaPoints


applyConvex :: [[Point]] -> [[Point]]
applyConvex list = Data.List.map convexHull list


orderPoints :: [Point] -> [Point]
orderPoints [] = []
orderPoints listOfPoints = [topLeft, topRight, bottomRight, bottomLeft]
	where
		listSum     = Data.List.map (\(x,y) -> x + y) listOfPoints
		bottomLeft  = listOfPoints !! (fromJust (elemIndex (minimum listSum) listSum))
		topRight    = listOfPoints !! (fromJust (elemIndex (maximum listSum) listSum))
		restList    = listOfPoints Data.List.\\ [bottomLeft, topRight]
		listDif     = Data.List.map (\(x,y) -> x - y) listOfPoints
		topLeft     = listOfPoints !! (fromJust (elemIndex (minimum listDif) listDif))
		bottomRight = listOfPoints !! (fromJust (elemIndex (maximum listDif) listDif))


getNewImageSize :: [Point] -> (Int, Int)
getNewImageSize pointsOfReceipt = (fromIntegral (ceiling (max widthA widthB)), fromIntegral (ceiling (max heightA heightB)))
	where
		topLeft     = pointsOfReceipt !! 0
		topRight    = pointsOfReceipt !! 1
		bottomRight = pointsOfReceipt !! 2
		bottomLeft  = pointsOfReceipt !! 3
		widthA      = distFunc topLeft topRight
		widthB      = distFunc bottomRight bottomLeft
		heightA     = distFunc topLeft bottomLeft
		heightB     = distFunc topRight bottomRight