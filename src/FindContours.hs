{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FindContours
    ( 
    findingContours
    ) where

import Utils
import Control.Monad ( void )
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Word
import Data.Proxy
import Data.Vector
import Control.Monad.Primitive
import qualified OpenCV as CV
import Linear.V2
import OpenCV.TypeLevel
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Core.Types.Size as S
import qualified OpenCV.ImgProc.StructuralAnalysis as SA
import GHC.Int (Int32)


findingContours :: PrimMonad m => M.Mat ('S '[h0, w0]) ('S 1) ('S Word8) -> m (Vector SA.Contour)
findingContours image = do
	imageM <- CV.thaw image
	contours_vector <- CV.findContours SA.ContourRetrievalList SA.ContourApproximationSimple imageM
	pure contours_vector