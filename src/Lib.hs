{-# LANGUAGE TypeFamilies #-}

module Lib
    ( controller
    ) where

import BlurImage
import ResizeImage
import CannyImage
import FindContours
import RightReceipt
import Utils
import Control.Monad ( void )
import Data.Word
import Data.List
import Data.Vector
import Data.Vector.Algorithms.Intro
import Foreign.C.Types
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV as CV
import qualified Data.ByteString as B
import GHC.Int (Int32)
import Linear.V2

--controller :: IO (CV.Mat (CV.S '[CV.D, CV.D]) (CV.S 1) (CV.S Word8))
controller :: IO()
controller = do
	--file <- B.readFile "/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0650.JPG"
	file <- B.readFile "/home/chuck/Pictures/rumpler_black.JPG"
	--file <- B.readFile "/home/chuck/Pictures/testingWithLetters.png"
	img <- return $ CV.imdecode CV.ImreadGrayscale file
	resized_little_img <- resizeImage img --little image for making a blur in and find the receipt
	blurred_Image <- blurImage ((CV.exceptError $ M.coerceMat resized_little_img) :: M.Mat (CV.S '[ CV.D, CV.D]) (CV.S 1) (CV.S Word8))
	cannied_Image <- cannyImage blurred_Image 
	contours <- (findingContours cannied_Image)
	let listOfPoints = getListOfPoints (Data.Vector.toList contours)
	let listOfAreas = dictAreaPoints listOfPoints

	--putStrLn (show (CV.contourArea (c :: Vector CV.Point2f)))
	--putStrLn (show (listOfAreas))
	putStrLn (show (findReceipt listOfAreas))
	--putStrLn (show (t + z))--}