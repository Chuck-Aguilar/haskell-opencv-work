module RightReceipt
    ( 
    	findReceipt
    ) where

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


--peri = cv2.arcLength(c, True)
--approx = cv2.approxPolyDP(c, 0.02 * peri, True)


--findReceipt :: [(Double, [(Int32, Int32)])] -> [(Int32, Int32)]
findReceipt :: [(Double, [(Int32, Int32)])] -> [Double]
findReceipt areaPoints = reverse (findPerimeter (fromList areaPoints))


approxPolyDP :: [(Int32, Int32)] -> Double -> [(Int32, Int32)] --List of points -> Epsilon (0.02 * perimeter) -> List of points
{-approxPolyDP pointList epsi = approxPolyDP' pointList epsi twoFarthest [] [] (twoFarthest !! 1)
	where
		twoFarthest = findFarthestTwoPoints pointList [(0,0), (0,0)] 0
		approxPolyDP' :: [(Int32, Int32)] -> Double -> [(Int32, Int32)] -> [(Int32, Int32)] -> [(Int32, Int32)] -> (Int32, Int32) -> [(Int32, Int32)]
		approxPolyDP' [x] epsi twoFarthest finalList usedList lastOne = lastOne : finalList
		approxPolyDP' pointList epsi twoFarthest finalList usedList lastOne			
			| pointLineDist > epsi = approxPolyDP' pointList epsi ([(twoFarthest !! 0), farthestPointToLine]) finalList usedList lastOne
			| otherwise = approxPolyDP' (drop (fromJust (elemIndex (twoFarthest !! 1) pointList)) pointList) epsi ([(twoFarthest !! 1), farthestPointRest]) ((twoFarthest !! 0) : finalList) (farthestPointToLine : (twoFarthest !! 0) : usedList) lastOne
			where
				farthestPointToLine = findFarthestPointToLine pointList twoFarthest (0, 0) 0
				pointLineDist = pointToLineDist (twoFarthest !! 0) (twoFarthest !! 1) farthestPointToLine
				farthestPoint = findFarthestPoint pointList (twoFarthest !! 1) (0, 0) 0
				farthestPointRest = findFarthestPoint (drop (fromJust (elemIndex (twoFarthest !! 1) pointList)) pointList) (twoFarthest !! 1) (0, 0) 0-}
approxPolyDP pointList epsi = approxPolyDP' pointList epsi (linkPoint : []) (rightPoint : []) []
	where
		points     = findLeftRightPoints pointList
		linkPoint  = head points
		rightPoint = last points
		approxPolyDP' :: [(Int32, Int32)] -> Double -> [(Int32, Int32)] -> [(Int32, Int32)] -> [(Int32, Int32)] -> [(Int32, Int32)]
		approxPolyDP' pointList epsi open [] final = final ++ open
		approxPolyDP' pointList epsi open closed final
			| pointLineDist > epsi = approxPolyDP' pointListInitTail epsi open (closed ++ [farthestPointToLine]) final 
			| otherwise            = approxPolyDP' pointListTail epsi (fromBToA open closed) farthestPointToLastPoint final
			where				
				farthestPointToLine = findFarthestPointToLine pointList [(last open), (last closed)] (0, 0) 0
				farthestPointToLastPoint = findFarthestPoint pointListTail (last closed) (0, 0) 0
				pointLineDist = pointToLineDist (last open) (last closed) farthestPointToLine
				pointListTail = drop (fromJust (elemIndex (last closed) pointList)) pointList
				pointListInitTail = (take (fromJust (elemIndex farthestPointToLine pointList)) pointList) ++ [farthestPointToLine]
				fromBToA xs ys = xs ++ [last ys]

findLeftRightPoints :: [(Int32, Int32)] -> [(Int32, Int32)]
findLeftRightPoints pointList = [(head sorted), (last sorted)]
	where
		sorted = sort pointList


pointToPointDist :: (Int32, Int32) -> (Int32, Int32) -> Double
pointToPointDist (x1, y1) (x2, y2) = sqrt (fromIntegral (x'^2 + y'^2))
	where
		x' = x1 - x2
		y' = y1 - y2


pointToLineDist :: (Int32, Int32) -> (Int32, Int32) -> (Int32, Int32) -> Double --First Line Point -> Second Line Point -> Point -> distance
pointToLineDist (x1, y1) (x2, y2) (x0, y0) = abs (fromIntegral ((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / pointToPointDist (x1, y1) (x2, y2))


findFarthestPoint :: [(Int32, Int32)] -> (Int32, Int32) -> (Int32, Int32) -> Double -> (Int32, Int32) --List of Points -> first Point -> farthest Point -> distance -> farthest Point
findFarthestPoint [] firstPoint finalPoint dist = finalPoint
findFarthestPoint (currntPoint : pointList) firstPoint finalPoint dist
	| pointToPointDist currntPoint firstPoint > dist = findFarthestPoint pointList firstPoint currntPoint (pointToPointDist currntPoint firstPoint) 
	| otherwise										 = findFarthestPoint pointList firstPoint finalPoint dist 


findFarthestTwoPoints :: [(Int32, Int32)] -> [(Int32, Int32)] -> Double -> [(Int32, Int32)] --List of Points -> [(Point, Point)] -> distance -> [(Point, Point)]
findFarthestTwoPoints [] finalList dist = finalList
findFarthestTwoPoints (currntPoint : pointList) finalList dist
	| newDist > dist = findFarthestTwoPoints pointList [currntPoint, farthestPoint] newDist
	| otherwise		  = findFarthestTwoPoints pointList finalList dist
	where		
		farthestPoint = findFarthestPoint pointList currntPoint currntPoint 0		
		newDist = pointToPointDist farthestPoint currntPoint	


findFarthestPointToLine :: [(Int32, Int32)]	-> [(Int32, Int32)]	-> (Int32, Int32) -> Double -> (Int32, Int32) --List of Points -> Line -> FartherstPoint -> distance -> FartherstPoint
findFarthestPointToLine [] line finalPoint dist = finalPoint
findFarthestPointToLine (currntPoint : pointList) line finalPoint dist
	| newDist > dist = findFarthestPointToLine pointList line currntPoint newDist
	| otherwise      = findFarthestPointToLine pointList line finalPoint dist
	where
		newDist = pointToLineDist (line !! 0) (line !! 1) currntPoint