{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_art (
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

bindir     = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/bin"
libdir     = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/lib/x86_64-osx-ghc-8.6.5/art-0.1.0.0-E7cn8AlFuh9EnPDdLakiRu-art-exe"
dynlibdir  = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/share/x86_64-osx-ghc-8.6.5/art-0.1.0.0"
libexecdir = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/libexec/x86_64-osx-ghc-8.6.5/art-0.1.0.0"
sysconfdir = "/Users/carlson/dev/haskell/GenerativeArt/art/.stack-work/install/x86_64-osx/lts-13.27/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "art_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "art_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "art_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "art_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "art_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "art_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
