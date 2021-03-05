{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Lab2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/eduardandrasuk/.cabal/bin"
libdir     = "/Users/eduardandrasuk/.cabal/lib/x86_64-osx-ghc-8.10.1/Lab2-0.1.0.0-inplace-Lab2-exe"
dynlibdir  = "/Users/eduardandrasuk/.cabal/lib/x86_64-osx-ghc-8.10.1"
datadir    = "/Users/eduardandrasuk/.cabal/share/x86_64-osx-ghc-8.10.1/Lab2-0.1.0.0"
libexecdir = "/Users/eduardandrasuk/.cabal/libexec/x86_64-osx-ghc-8.10.1/Lab2-0.1.0.0"
sysconfdir = "/Users/eduardandrasuk/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Lab2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lab2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Lab2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Lab2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
