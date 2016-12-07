module ApproxPolyDP
	(
		minifyPolygon		
	) where

import RamerDouglasPeuckerParts
import Data.Maybe
import Data.List
import Control.Monad

init_iters      = 3
new_right_slice = [0,0]
new_start_point = (0,0)
new_slice       = [0,0]
new_all         = All [] []

minifyPolygon :: [Point] -> Double -> [Point]
minifyPolygon srcContours epsi = do
	let fFP   = findFarthestPoints srcContours init_iters 0 0 eps new_right_slice count 0 new_start_point
	let iST   = initStack srcContours (myfst fFP) new_slice (mysnd fFP) (trd fFP) count new_all
	let final = recurProcess iST srcContours eps (getRightSliceFromStack iST) count
	final
	where
		eps   = epsi * epsi
		count = length srcContours


