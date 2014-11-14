module Paths_Wacc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/redhead/.cabal/bin"
libdir     = "/home/redhead/.cabal/lib/Wacc-0.3/ghc-7.4.1"
datadir    = "/home/redhead/.cabal/share/Wacc-0.3"
libexecdir = "/home/redhead/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Wacc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Wacc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Wacc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Wacc_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
