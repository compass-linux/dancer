module Dancer.Fetch where

import System.Process (callProcess, readProcess, system)
import System.Exit (ExitCode(..))
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>), takeDirectory)
import Data.List (isPrefixOf)
import Dancer.Types
import Dancer.Logging

sourcesCacheDir :: FilePath
sourcesCacheDir = "/var/lib/dancer/cache/sources"

fetchPackageSource :: Package -> IO FilePath
fetchPackageSource pkg = do
  logSubStep $ "Fetching " ++ pkgName pkg
  
  let cacheDir = sourcesCacheDir </> pkgName pkg </> pkgVersion pkg
  
  case pkgSource pkg of
    GitSource url branch -> do
      logProgress $ "Cloning from " ++ url
      fetchGitSource pkg url branch cacheDir
    LocalSource path -> do
      logProgress $ "Using local source at " ++ path
      return path

fetchGitSource :: Package -> String -> String -> FilePath -> IO FilePath
fetchGitSource pkg url branch cacheDir = do
  createDirectoryIfMissing True sourcesCacheDir
  
  exists <- doesDirectoryExist cacheDir
  
  if exists
    then do
      logProgress "Source already cached, updating..."
      let gitCmd = "cd " ++ cacheDir ++ " && git fetch origin " ++ branch
      exitCode <- system gitCmd
      case exitCode of
        ExitSuccess -> do
          logOK $ "Updated " ++ pkgName pkg
          return cacheDir
        _ -> do
          logFail $ "Failed to update " ++ pkgName pkg
          removeDirectoryRecursive cacheDir
          cloneGitRepo url branch cacheDir pkg
    else do
      logProgress $ "Cloning repository..."
      cloneGitRepo url branch cacheDir pkg

cloneGitRepo :: String -> String -> FilePath -> Package -> IO FilePath
cloneGitRepo url branch cacheDir pkg = do
  createDirectoryIfMissing True (takeDirectory cacheDir)
  
  let gitCmd = "git clone --branch " ++ branch ++ " " ++ url ++ " " ++ cacheDir
  exitCode <- system gitCmd
  
  case exitCode of
    ExitSuccess -> do
      logOK $ "Cloned " ++ pkgName pkg
      return cacheDir
    _ -> do
      logFail $ "Failed to clone " ++ pkgName pkg ++ " from " ++ url
      error $ "Git clone failed for " ++ pkgName pkg

fetchAllSources :: [Package] -> IO [FilePath]
fetchAllSources pkgs = do
  logStep "Fetching package sources"
  mapM fetchPackageSource pkgs
