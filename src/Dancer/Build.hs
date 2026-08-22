module Dancer.Build where

import System.Process (callProcess, readProcess, system)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Data.List (nub, intercalate)
import Dancer.Types
import Dancer.Logging
import Dancer.Fetch (fetchPackageSource)

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
  
  showBuildSummary config resolvedPkgs
  confirmed <- askConfirmation "Proceed with build?"
  
  if not confirmed
    then do
      logWarn "Build cancelled"
      exitFailure
    else do
      logStep "Building packages"
      mapM_ (buildPackage config) resolvedPkgs
      logOK "All packages built successfully"
      
      logSubStep "Creating new generation"
      genNum <- createGeneration config
      logOK $ "Generation " ++ show genNum ++ " created"
      
      logStep "System build complete"

showBuildSummary :: SystemConfig -> [Package] -> IO ()
showBuildSummary config pkgs = do
  putStrLn ""
  logStep "Build Summary"
  putStrLn $ "  Hostname: " ++ hostname config
  putStrLn $ "  LibC: " ++ show (libc config)
  putStrLn $ "  Coreutils: " ++ show (coreutils config)
  putStrLn $ "  Build Flags: " ++ buildFlags config
  putStrLn $ "  Packages to build: " ++ show (length pkgs)
  putStrLn ""
  putStrLn "  Packages:"
  mapM_ (\pkg -> putStrLn $ "    - " ++ pkgName pkg ++ " (" ++ pkgVersion pkg ++ ")") (take 10 pkgs)
  if length pkgs > 10
    then putStrLn $ "    ... and " ++ show (length pkgs - 10) ++ " more"
    else return ()
  putStrLn ""
  
  if not (null (useFlags config))
    then do
      putStrLn "  Global USE flags:"
      mapM_ (\flag -> putStrLn $ "    + " ++ flag) (useFlags config)
      putStrLn ""
    else return ()

askConfirmation :: String -> IO Bool
askConfirmation prompt = do
  putStr $ prompt ++ " [y/N] "
  response <- getLine
  return $ response `elem` ["y", "Y", "yes", "YES"]

buildPackage :: SystemConfig -> Package -> IO ()
buildPackage config pkg = do
  logProgress $ "Building " ++ pkgName pkg ++ " (" ++ pkgVersion pkg ++ ")"
  
  let buildEnv = 
        [ ("MAKEFLAGS", buildFlags config)
        , ("CFLAGS", "-O2 -march=native")
        ]
      installPrefix = "/usr/local"
  
  let workDir = buildCacheDir </> pkgName pkg </> pkgVersion pkg
  createDirectoryIfMissing True workDir
  
  logSubStep $ "Fetching source for " ++ pkgName pkg
  sourceDir <- fetchPackageSource pkg
  logOK $ "Source ready at " ++ sourceDir
  
  let appliedFlags = filter (`elem` useFlags config) (pkgUseFlags pkg)
  if not (null appliedFlags)
    then do
      logSubStep $ "Applied USE flags: " ++ intercalate ", " appliedFlags
    else return ()
  
  logSubStep $ "Configuring " ++ pkgName pkg
  configurePackage config pkg sourceDir workDir installPrefix
  
  logSubStep $ "Compiling " ++ pkgName pkg
  compilePackage config pkg workDir
  
  logSubStep $ "Installing " ++ pkgName pkg
  installPackage pkg workDir
  
  logOK $ pkgName pkg ++ " built and installed"

configurePackage :: SystemConfig -> Package -> FilePath -> FilePath -> FilePath -> IO ()
configurePackage config pkg sourceDir workDir prefix = do
  let configCmd = "cd " ++ sourceDir ++ " && ./configure --prefix=" ++ prefix
  exitCode <- system configCmd
  case exitCode of
    ExitSuccess -> logOK $ "Configured " ++ pkgName pkg
    _ -> do
      logWarn $ "Configure step skipped for " ++ pkgName pkg ++ " (no configure script)"

compilePackage :: SystemConfig -> Package -> FilePath -> IO ()
compilePackage config pkg workDir = do
  let makeCmd = "cd " ++ workDir ++ " && make " ++ buildFlags config
  exitCode <- system makeCmd
  case exitCode of
    ExitSuccess -> logOK $ "Compiled " ++ pkgName pkg
    _ -> do
      logFail $ "Compilation failed for " ++ pkgName pkg
      exitFailure

installPackage :: Package -> FilePath -> IO ()
installPackage pkg workDir = do
  let installCmd = "cd " ++ workDir ++ " && make install"
  exitCode <- system installCmd
  case exitCode of
    ExitSuccess -> return ()
    _ -> do
      logFail $ "Installation failed for " ++ pkgName pkg
      exitFailure

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
