module Paths_simple (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chuck/Documents/simple/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/bin"
libdir     = "/home/chuck/Documents/simple/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/lib/x86_64-linux-ghc-7.10.3/simple-0.1.0.0-3KtLGDpFPvjB6VnwWh4q2E"
datadir    = "/home/chuck/Documents/simple/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/share/x86_64-linux-ghc-7.10.3/simple-0.1.0.0"
libexecdir = "/home/chuck/Documents/simple/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/libexec"
sysconfdir = "/home/chuck/Documents/simple/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "simple_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
