{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tkani_api
  
  (
    version,
    getBinDir,
    getLibDir,
    getDynLibDir,
    getLibexecDir,
    getDataFileName,
    getDataDir,
    getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)




bindir     :: FilePath
bindir     = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/bin"
getBinDir     :: IO FilePath
getBinDir     = catchIO (getEnv "tkani_api_bindir")     (\_ -> return bindir)

libdir     :: FilePath
libdir     = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/lib/aarch64-osx-ghc-9.10.3-6621/tkani-api-0.1.0.0-KoRyXGrFacc5tgGl20blP1-tkani-api-exe"
getLibDir     :: IO FilePath
getLibDir     = catchIO (getEnv "tkani_api_libdir")     (\_ -> return libdir)

dynlibdir  :: FilePath
dynlibdir  = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/lib/aarch64-osx-ghc-9.10.3-6621"
getDynLibDir  :: IO FilePath
getDynLibDir  = catchIO (getEnv "tkani_api_dynlibdir")  (\_ -> return dynlibdir)

datadir    :: FilePath
datadir    = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/share/aarch64-osx-ghc-9.10.3-6621/tkani-api-0.1.0.0"
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "tkani_api_datadir")    (\_ -> return datadir)

libexecdir :: FilePath
libexecdir = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/libexec/aarch64-osx-ghc-9.10.3-6621/tkani-api-0.1.0.0"
getLibexecDir :: IO FilePath
getLibexecDir = catchIO (getEnv "tkani_api_libexecdir") (\_ -> return libexecdir)

sysconfdir :: FilePath
sysconfdir = "/Users/sonny/tkani/tkani-api/.stack-work/install/aarch64-osx-nix/0c7c389fe02fb9486b4fc5a16053aa4a100df25300f4d1eea3402607b43ba2e9/9.10.3/etc"
getSysconfDir :: IO FilePath
getSysconfDir = catchIO (getEnv "tkani_api_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
