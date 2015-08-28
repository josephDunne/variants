module Paths_variants (
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

bindir     = "/Users/jo/.cabal/bin"
libdir     = "/Users/jo/.cabal/lib/x86_64-osx-ghc-7.10.2/varia_5wCMPNeYlGhAk0qTYbHvsm"
datadir    = "/Users/jo/.cabal/share/x86_64-osx-ghc-7.10.2/variants-0.1.0.0"
libexecdir = "/Users/jo/.cabal/libexec"
sysconfdir = "/Users/jo/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "variants_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "variants_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "variants_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "variants_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "variants_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
