module Paths_safeblaze (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2011,12], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jhiesey/Library/Haskell/ghc-7.2.2/lib/safeblaze-2011.12/bin"
libdir     = "/Users/jhiesey/Library/Haskell/ghc-7.2.2/lib/safeblaze-2011.12/lib"
datadir    = "/Users/jhiesey/Library/Haskell/ghc-7.2.2/lib/safeblaze-2011.12/share"
libexecdir = "/Users/jhiesey/Library/Haskell/ghc-7.2.2/lib/safeblaze-2011.12/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "safeblaze_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "safeblaze_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "safeblaze_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "safeblaze_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
