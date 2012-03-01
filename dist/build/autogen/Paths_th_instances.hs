module Paths_th_instances (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0,10], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/hi5networks/.cabal/bin"
libdir     = "/Users/hi5networks/.cabal/lib/th-instances-0.1.0.10/ghc-7.2.2"
datadir    = "/Users/hi5networks/.cabal/share/th-instances-0.1.0.10"
libexecdir = "/Users/hi5networks/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "th_instances_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "th_instances_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "th_instances_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "th_instances_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
