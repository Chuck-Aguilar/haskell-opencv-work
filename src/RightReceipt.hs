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

findPerimeter :: Map Double [(Int32, Int32)] -> [Double]
findPerimeter areaPoints = Data.Map.foldl (\acc x -> acc ++ [getPerimeter x]) [] areaPoints

getPerimeter :: [(Int32, Int32)] -> Double
getPerimeter (firstPoint : points) = getPerimeter' points firstPoint firstPoint 0
	where
		getPerimeter' :: [(Int32, Int32)] -> (Int32, Int32) -> (Int32, Int32) -> Double -> Double
		getPerimeter' [] (crrntX, crrntY) (prevX, prevY) acc = acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2)))
		getPerimeter' ((crrntX, crrntY) : point) firstPoint (prevX, prevY) acc = getPerimeter' point firstPoint (crrntX, crrntY) (acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2))))



--findReceipt areaPoints = findAllPolyDp (getListOfPoints mapAreaPoints) epsilons
--findReceipt areaPoints = Data.List.map (length) (findAllPolyDp (applyConvex (getListOfPoints mapAreaPoints)) epsilons)
--findReceipt areaPoints = Data.List.map (length) (findAllPolyDp ((getListOfPoints mapAreaPoints)) epsilons)
findReceipt areaPoints = findAllPolyDp ((getListOfPoints mapAreaPoints)) epsilons
--findReceipt areaPoints = epsilons
--findReceipt areaPoints = applyConvex (getListOfPoints mapAreaPoints)
--findReceipt areaPoints = (((getListOfPoints mapAreaPoints) !! 3), (epsilons !! 3))

	where
		mapAreaPoints = fromList areaPoints
		epsilons = findEpsilon (reverse (findPerimeter mapAreaPoints))		

findEpsilon :: [Double] -> [Double]
findEpsilon xs = Data.List.map (* 0.02) xs

findAllPolyDp :: [[(Int32, Int32)]] -> [Double] -> [[(Int32, Int32)]]
findAllPolyDp listOfPoints epsilons = Data.List.map (uncurry minifyPolygon) (zip listOfPoints epsilons)

getListOfPoints :: Map Double [(Int32, Int32)] -> [[(Int32, Int32)]]
getListOfPoints areaPoints = Data.Map.foldl (\acc x -> x : acc) [] areaPoints

applyConvex :: [[(Int32, Int32)]] -> [[(Int32, Int32)]]
applyConvex list = Data.List.map convexHull list

