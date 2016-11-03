import Data.Maybe
import Data.List

data Users = Height Int | Age Int deriving (Show)

older :: Users -> Users
older (Age a) = Age (a+1)
older _       = error "not Age"

everyOneOlder :: [Users] -> [Users]
everyOneOlder li = [older x | x <- li]

getPerimeter :: [(Int, Int)] -> Double
getPerimeter (firstPoint : points) = getPerimeter' points firstPoint firstPoint 0
	where
		getPerimeter' :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Double -> Double
		getPerimeter' [] (crrntX, crrntY) (prevX, prevY) acc = acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2)))
		getPerimeter' ((crrntX, crrntY) : point) firstPoint (prevX, prevY) acc = getPerimeter' point firstPoint (crrntX, crrntY) (acc + (sqrt (fromIntegral ((crrntX - prevX) ^ 2 + (crrntY - prevY) ^ 2))))

approxPolyDP :: [(Int, Int)] -> Double -> [(Int, Int)] --List of points -> Epsilon (0.02 * perimeter) -> List of points
approxPolyDP pointList epsi = approxPolyDP' pointList epsi (linkPoint : []) (rightPoint : []) []
	where
		points     = findLeftRightPoints pointList
		linkPoint  = head points
		rightPoint = last points
		approxPolyDP' :: [(Int, Int)] -> Double -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
		approxPolyDP' pointList epsi open [] final = final ++ open
		approxPolyDP' pointList epsi open closed final
			| pointLineDist > epsi = approxPolyDP' pointListInitTail epsi open (closed ++ [farthestPointToLine]) final 
			| otherwise            = approxPolyDP' pointListTail epsi (fromBToA open closed) [farthestPointToLastPoint] final
			where				
				farthestPointToLine = findFarthestPointToLine pointList [(last open), (last closed)] (0, 0) 0
                                farthestPointToLastPoint = findFarthestPoint pointListTail (last closed) (0, 0) 0
				pointLineDist = pointToLineDist (last open) (last closed) farthestPointToLine
				pointListTail = drop (fromJust (elemIndex (last closed) pointList)) pointList
                                pointListInitTail = (take (fromJust (elemIndex farthestPointToLine pointList)) pointList) ++ [farthestPointToLine]
				fromBToA xs ys = xs ++ [last ys]


findLeftRightPoints :: [(Int, Int)] -> [(Int, Int)]
findLeftRightPoints pointList = [(head sorted), (last sorted)]
	where
		sorted = sort pointList


pointToPointDist :: (Int, Int) -> (Int, Int) -> Double
pointToPointDist (x1, y1) (x2, y2) = sqrt (fromIntegral (x'^2 + y'^2))
	where
		x' = x1 - x2
		y' = y1 - y2

pointToLineDist :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
pointToLineDist (x1, y1) (x2, y2) (x0, y0) = abs (fromIntegral ((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / pointToPointDist (x1, y1) (x2, y2))

findFarthestPoint :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Double -> (Int, Int)
findFarthestPoint [] firstPoint finalPoint dist = finalPoint
findFarthestPoint (currntPoint : pointList) firstPoint finalPoint dist
	| pointToPointDist currntPoint firstPoint > dist = findFarthestPoint pointList firstPoint currntPoint (pointToPointDist currntPoint firstPoint) 
	| otherwise					 = findFarthestPoint pointList firstPoint finalPoint dist 


findFarthestTwoPoints :: [(Int, Int)] -> [(Int, Int)] -> Double -> [(Int, Int)]
findFarthestTwoPoints [] finalList dist = finalList
findFarthestTwoPoints (currntPoint : pointList) finalList dist
	|  newDist > dist = findFarthestTwoPoints pointList [currntPoint, farthestPoint] newDist
	| otherwise		  = findFarthestTwoPoints pointList finalList dist
	where		
		farthestPoint = findFarthestPoint pointList currntPoint currntPoint 0		
		newDist = pointToPointDist farthestPoint currntPoint		

findFarthestPointToLine :: [(Int, Int)]	-> [(Int, Int)]	-> (Int, Int) -> Double -> (Int, Int)
findFarthestPointToLine [] line finalPoint dist = finalPoint
findFarthestPointToLine (currntPoint : pointList) line finalPoint dist
	| newDist > dist = findFarthestPointToLine pointList line currntPoint newDist
	| otherwise      = findFarthestPointToLine pointList line finalPoint dist
	where
		newDist = pointToLineDist (line !! 0) (line !! 1) currntPoint
				
