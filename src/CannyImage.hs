{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CannyImage
    ( 
    cannyImage
    ) where

import Utils
import Control.Monad ( void )
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Word
import Data.Proxy
import qualified OpenCV as CV
import Linear.V2
import OpenCV.TypeLevel
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Core.Types.Size as S
import qualified OpenCV.ImgProc.GeometricImgTransform as GIT
import GHC.Int (Int32)


canny :: M.Mat ('S '[h0, w0]) ('S 1) ('S Word8) -> CV.CvExcept (M.Mat ('S '[h0, w0]) ('S 1) ('S Word8)) 
--canny image = CV.canny 75 200 Nothing CV.CannyNormL1 image
canny image = CV.canny 10 200 Nothing CV.CannyNormL1 image

cannyImage :: M.Mat ('S '[h0, w0]) ('S 1) ('S Word8) -> IO (M.Mat ('S '[h0, w0]) ('S 1) ('S Word8))
cannyImage image = do
    canniedImage   <- return $ canny image     
    return $ CV.exceptError $ canniedImage