module Main where

import Lib
import qualified OpenCV.Internal.Core.Types.Mat as M
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified Data.ByteString as B

main :: IO ()
main = do
	controller
    {-test <- controller
    CV.withWindow "test" $ \window -> do
        CV.imshow window test 
        void $ CV.waitKey 0-}
