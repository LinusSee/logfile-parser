{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_parser_rest (
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

bindir     = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\bin"
libdir     = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\lib\\x86_64-windows-ghc-8.8.4\\parser-rest-0.1.0.0-3wT36pk5EblA6SKImdBb8P-parser-rest-exe"
dynlibdir  = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\share\\x86_64-windows-ghc-8.8.4\\parser-rest-0.1.0.0"
libexecdir = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\libexec\\x86_64-windows-ghc-8.8.4\\parser-rest-0.1.0.0"
sysconfdir = "D:\\projects\\logfile-parser\\parser-rest\\.stack-work\\install\\a13b0abc\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parser_rest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser_rest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "parser_rest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "parser_rest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser_rest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parser_rest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
