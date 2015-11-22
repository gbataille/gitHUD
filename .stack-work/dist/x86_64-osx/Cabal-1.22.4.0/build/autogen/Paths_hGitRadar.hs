module Paths_hGitRadar (
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

bindir     = "/Users/gbataille/Documents/Prog/Perso/hGitRadar/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/bin"
libdir     = "/Users/gbataille/Documents/Prog/Perso/hGitRadar/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/lib/x86_64-osx-ghc-7.10.2/hGitRadar-0.1.0.0-Coe1kEoKJmCHiIZnwYE7kZ"
datadir    = "/Users/gbataille/Documents/Prog/Perso/hGitRadar/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/share/x86_64-osx-ghc-7.10.2/hGitRadar-0.1.0.0"
libexecdir = "/Users/gbataille/Documents/Prog/Perso/hGitRadar/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/libexec"
sysconfdir = "/Users/gbataille/Documents/Prog/Perso/hGitRadar/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hGitRadar_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hGitRadar_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hGitRadar_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hGitRadar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hGitRadar_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
