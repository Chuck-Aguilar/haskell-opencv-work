module Utils
    ( 
      getHandW
    , getSize
    , getImageFromEither
    , getListOfPoints
    , dictAreaPoints
    ) where

import Control.Monad ( void )
import Data.List
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import GHC.Int (Int32)
import Linear.V2

getHandW image = M.miShape $ M.matInfo image

getMonValue :: Maybe Int -> Int
getMonValue (Just x) = x

fromFractToInt :: (Num b, RealFrac a) => a -> b
fromFractToInt x = fromIntegral (truncate x) 

fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

getSize :: Int32 -> Int32 -> Maybe Int -> Maybe Int -> V2 Int32
getSize w h wanted_w wanted_y 
    | wanted_w == Nothing = V2 (fromIntegral(((truncate ((fromIntegral w) * r1))))) (fromIntegral(getMonValue wanted_y))
    | wanted_y == Nothing = V2 (fromIntegral(getMonValue wanted_w)) (fromIntegral(((truncate ((fromIntegral h) * r2)))))
    | otherwise           = error "Either wanted_w or wanted_y should be a value"
    where 
        r1 = (fromIntegral (getMonValue wanted_y) / fromIntegral h)
        r2 = (fromIntegral (getMonValue wanted_w) / fromIntegral w)

getImageFromEither :: Either a b -> b
getImageFromEither eitherImage = fromRight eitherImage

getListOfPoints :: [CV.Contour] -> [[(Int32, Int32)]]
getListOfPoints vector = foldl (\acc x -> acc ++ [getContoursPoints x]) [] vector
    where
        getContoursPoints :: CV.Contour -> [(Int32, Int32)]
        getContoursPoints (CV.Contour c k) = foldl (\acc x -> acc ++ [getVectorPoints x]) [] c
            where
                getVectorPoints :: CV.Point 2 Int32 -> (Int32, Int32)
                getVectorPoints toPointFunc = extractPoints ((CV.fromPoint (toPointFunc :: CV.Point 2 Int32)) :: V2 Int32)
                    where 
                        extractPoints :: V2 Int32 -> (Int32, Int32)
                        extractPoints (V2 a b) = (a, b)

polygonArea :: [(Int32, Int32)] -> Int32
polygonArea (firstPoint : polygon) = polygonArea' polygon firstPoint firstPoint 0
    where
        polygonArea' :: [(Int32, Int32)] -> (Int32, Int32) -> (Int32, Int32) -> Int32 -> Int32
        polygonArea' [] (x, y) (x1, y1) acc = (acc + (x1 * y - y1 * x))
        polygonArea' ((crrntX, crrntY) : polygon) firstPoint (prevX, prevY) acc = polygonArea' polygon firstPoint (crrntX, crrntY) (acc + ((prevX * crrntY - prevY * crrntX)))

polygonFinalArea :: [[(Int32, Int32)]] -> [Double]
polygonFinalArea polygon = map abs (foldl (\acc x -> acc ++ [(fromIntegral x * 0.5 )]) [] (map polygonArea polygon))

sortingAreas :: [(Double, [(Int32, Int32)])] -> [(Double, [(Int32, Int32)])]
sortingAreas listOfAreas = sortBy (\x y -> if x > y then LT else GT) listOfAreas

dictAreaPoints :: [[(Int32, Int32)]] -> [(Double, [(Int32, Int32)])]
dictAreaPoints polygon = take 10 (sortingAreas (zip (polygonFinalArea polygon) polygon))


