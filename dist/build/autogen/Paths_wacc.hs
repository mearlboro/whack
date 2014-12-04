module Paths_wacc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tudor/.cabal/bin"
libdir     = "/home/tudor/.cabal/lib/wacc-0.3/ghc-7.6.3"
datadir    = "/home/tudor/.cabal/share/wacc-0.3"
libexecdir = "/home/tudor/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wacc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wacc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wacc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wacc_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
