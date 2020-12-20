{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Tetris_hs (
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

bindir     = "/home/angelica/.cabal/bin"
libdir     = "/home/angelica/.cabal/lib/x86_64-linux-ghc-8.6.5/Tetris-hs-0.1.0.0-1JbhBI2FynM27qSGJmDMkK"
dynlibdir  = "/home/angelica/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/angelica/.cabal/share/x86_64-linux-ghc-8.6.5/Tetris-hs-0.1.0.0"
libexecdir = "/home/angelica/.cabal/libexec/x86_64-linux-ghc-8.6.5/Tetris-hs-0.1.0.0"
sysconfdir = "/home/angelica/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tetris_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tetris_hs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Tetris_hs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Tetris_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tetris_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tetris_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
