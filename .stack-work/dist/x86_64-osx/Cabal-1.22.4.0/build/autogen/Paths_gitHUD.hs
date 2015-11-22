module Paths_gitHUD (
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

bindir     = "/Users/gbataille/Documents/Prog/Perso/gitHUD/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/bin"
libdir     = "/Users/gbataille/Documents/Prog/Perso/gitHUD/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/lib/x86_64-osx-ghc-7.10.2/gitHUD-0.1.0.0-9QTX0F9BF7B1JPoxvWfB8P"
datadir    = "/Users/gbataille/Documents/Prog/Perso/gitHUD/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/share/x86_64-osx-ghc-7.10.2/gitHUD-0.1.0.0"
libexecdir = "/Users/gbataille/Documents/Prog/Perso/gitHUD/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/libexec"
sysconfdir = "/Users/gbataille/Documents/Prog/Perso/gitHUD/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gitHUD_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gitHUD_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gitHUD_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gitHUD_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gitHUD_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
