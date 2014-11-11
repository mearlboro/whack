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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/homes/ms6413/.cabal/bin"
libdir     = "/homes/ms6413/.cabal/lib/wacc-0.1/ghc-7.6.3"
datadir    = "/homes/ms6413/.cabal/share/wacc-0.1"
libexecdir = "/homes/ms6413/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wacc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wacc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wacc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wacc_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
