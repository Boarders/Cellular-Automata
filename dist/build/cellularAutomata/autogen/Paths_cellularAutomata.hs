{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cellularAutomata (
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

bindir     = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/bin"
libdir     = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.1/cellularAutomata-0.1.0.0-DFOULUKZtqJ3ctDM2YT1uG-cellularAutomata"
dynlibdir  = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/share/x86_64-osx-ghc-8.2.1/cellularAutomata-0.1.0.0"
libexecdir = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.1/cellularAutomata-0.1.0.0"
sysconfdir = "/Users/callanmcgill/Documents/Haskell/cellularAutomata/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cellularAutomata_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cellularAutomata_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cellularAutomata_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cellularAutomata_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cellularAutomata_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cellularAutomata_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
