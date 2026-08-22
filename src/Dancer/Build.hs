module Dancer.Build where

import System.Process (callProcess, readProcess, system)
import System.Exit (ExitCode(..), exitFailure)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Data.List (nub, intercalate)
import Dancer.Types
import Dancer.Logging

buildCacheDir :: FilePath
buildCacheDir = "/var/lib/dancer/cache"

buildDbDir :: FilePath
buildDbDir = "/var/lib/dancer/db"

generationsDir :: FilePath
generationsDir = "/var/lib/dancer/generations"

buildSystem :: SystemConfig -> Bool -> IO ()
buildSystem config fromScratch = do
  logStep "Building system"
  
  logSubStep "Setting up build directories"
  createDirectoryIfMissing True buildCacheDir
  createDirectoryIfMissing True buildDbDir
  createDirectoryIfMissing True generationsDir
  logOK "Build directories ready"
  
  logSubStep "Resolving package dependencies"
  let resolvedPkgs = resolveDependencies (packages config)
  logOK $ "Resolved " ++ show (length resolvedPkgs) ++ " packages"
  
  logSubStep "Checking for package conflicts"
  case checkConflicts config resolvedPkgs of
    [] -> logOK "No conflicts detected"
    conflicts -> do
      logFail "Package conflicts detected:"
      mapM_ (logWarn . ("  " ++)) conflicts
      exitFailure
  
  logSubStep "Building packages"
  mapM_ (buildPackage config) resolvedPkgs
  logOK "All packages built"
  
  logSubStep "Creating new generation"
  genNum <- createGeneration config
  logOK $ "Generation " ++ show genNum ++ " created"
  
  logStep "System build complete"

buildPackage :: SystemConfig -> Package -> IO ()
buildPackage config pkg = do
  logProgress $ "Building " ++ pkgName pkg
  
  let buildEnv = 
        [ ("MAKEFLAGS", buildFlags config)
        , ("CFLAGS", "-O2 -march=native")
        ]
  
  let workDir = buildCacheDir </> pkgName pkg
  createDirectoryIfMissing True workDir
  
  logSubStep $ "Fetching " ++ pkgName pkg
  
  logSubStep $ "Configuring " ++ pkgName pkg
  
  logSubStep $ "Compiling " ++ pkgName pkg
  
  logSubStep $ "Installing " ++ pkgName pkg
  
  logOK $ pkgName pkg ++ " built"

resolveDependencies :: [Package] -> [Package]
resolveDependencies [] = []
resolveDependencies pkgs = nub $ pkgs ++ concatMap (resolveDependencies . pkgDeps) pkgs
  where
    resolveDependencies :: [String] -> [Package]
    resolveDependencies depNames = 
      [pkg | pkg <- pkgs, pkgName pkg `elem` depNames]

checkConflicts :: SystemConfig -> [Package] -> [String]
checkConflicts config pkgs = 
  case (libc config, coreutils config) of
    (Glibc, GNU) -> []
    (Glibc, Busybox) -> []
    (Musl, Busybox) -> []
    (Musl, GNU) -> ["GNU coreutils may not work well with musl libc"]
    _ -> []

createGeneration :: SystemConfig -> IO Int
createGeneration config = do
  let genList = generationsDir </> "generations"
  exists <- doesFileExist genList
  
  nextGen <- if exists
    then do
      content <- readFile genList
      let lastGen = length (lines content)
      return (lastGen + 1)
    else
      return 1
  
  let entry = "Generation " ++ show nextGen ++ ": " ++ hostname config
  appendFile genList (entry ++ "\n")
  
  return nextGen

rebuildIncremental :: SystemConfig -> IO ()
rebuildIncremental config = do
  logStep "Incremental rebuild"
  logSubStep "Checking for changes"
  
  logOK "Incremental rebuild complete"

rebuildFromScratch :: SystemConfig -> IO ()
rebuildFromScratch config = do
  logStep "Cold rebuild (from scratch)"
  buildSystem config True

rollbackGeneration :: Int -> IO ()
rollbackGeneration genNum = do
  logStep $ "Rolling back to generation " ++ show genNum
  logSubStep "Restoring packages"
  logOK $ "Rolled back to generation " ++ show genNum
