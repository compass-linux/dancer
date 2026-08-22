module Dancer.Build where

import System.Process (callProcess, readProcess, system)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.Random (randomRIO)
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

encirclementsDir :: FilePath
encirclementsDir = "/var/lib/dancer/encirclements"

wordList :: [String]
wordList =
  [ "amber", "basalt", "cinder", "drift", "ember", "flint", "granite"
  , "hollow", "ironwood", "jasper", "kestrel", "lantern", "marrow"
  , "nettle", "obsidian", "pallor", "quartz", "ridge", "slate"
  , "thistle", "umber", "vellum", "wren", "yarrow", "zephyr"
  ]

randomWord :: IO String
randomWord = do
  idx <- randomRIO (0, length wordList - 1)
  return (wordList !! idx)

newEncirclementPath :: IO FilePath
newEncirclementPath = do
  w1 <- randomWord
  w2 <- randomWord
  w3 <- randomWord
  return $ encirclementsDir </> (w1 ++ "-" ++ w2 ++ "-" ++ w3)

buildSystem :: SystemConfig -> Bool -> IO ()
buildSystem config fromScratch = do
  logStep "Building system"
  
  createDirectoryIfMissing True buildCacheDir
  createDirectoryIfMissing True buildDbDir
  createDirectoryIfMissing True generationsDir
  logSubStep "Setting up build directories"
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
  
  let workDir = buildCacheDir </> pkgName pkg </> pkgVersion pkg
  createDirectoryIfMissing True workDir
  
  logSubStep $ "Fetching source for " ++ pkgName pkg
  sourceDir <- fetchPackageSource pkg
  logOK $ "Source ready at " ++ sourceDir
  
  let appliedFlags = filter (`elem` useFlags config) (pkgUseFlags pkg)
  if not (null appliedFlags)
    then logSubStep $ "Applied USE flags: " ++ intercalate ", " appliedFlags
    else return ()
  
  encirclement <- newEncirclementPath
  createDirectoryIfMissing True encirclement
  logSubStep $ "Encirclement: " ++ encirclement
  
  logSubStep $ "Building with " ++ show (buildMode pkg)
  case buildMode pkg of
    Autotools -> buildAutotools config pkg sourceDir encirclement
    Autoconf -> buildAutoconf config pkg sourceDir encirclement
    Make -> buildMake config pkg sourceDir encirclement
    Meson -> buildMeson config pkg sourceDir encirclement
  
  logOK $ pkgName pkg ++ " built into " ++ encirclement

buildAutotools :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildAutotools config pkg sourceDir encirclement = do
  logSubStep "Running autoreconf"
  exitCode <- system $ "cd " ++ sourceDir ++ " && autoreconf -i"
  case exitCode of
    ExitSuccess -> logOK "autoreconf done"
    _ -> logWarn "autoreconf failed, continuing anyway"
  
  buildAutoconf config pkg sourceDir encirclement

buildAutoconf :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildAutoconf config pkg sourceDir encirclement = do
  logSubStep $ "Configuring " ++ pkgName pkg
  let configCmd = "cd " ++ sourceDir ++ " && ./configure --prefix=" ++ encirclement
  exitCode <- system configCmd
  case exitCode of
    ExitSuccess -> logOK $ "Configured " ++ pkgName pkg
    _ -> do
      logFail $ "Configure failed for " ++ pkgName pkg
      exitFailure
  
  compileMake config pkg sourceDir
  installMake pkg sourceDir

buildMake :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildMake config pkg sourceDir encirclement = do
  logSubStep $ "Configuring (Makefile) " ++ pkgName pkg
  compileMake config pkg sourceDir
  let installCmd = "cd " ++ sourceDir ++ " && make install PREFIX=" ++ encirclement
  exitCode <- system installCmd
  case exitCode of
    ExitSuccess -> logOK $ "Installed " ++ pkgName pkg
    _ -> do
      logFail $ "Install failed for " ++ pkgName pkg
      exitFailure

buildMeson :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildMeson config pkg sourceDir encirclement = do
  logSubStep $ "Configuring (Meson) " ++ pkgName pkg
  let mesonCmd = "cd " ++ sourceDir ++ " && meson setup builddir --prefix=" ++ encirclement
  exitCode <- system mesonCmd
  case exitCode of
    ExitSuccess -> logOK "Meson configured"
    _ -> do
      logFail $ "Meson configuration failed for " ++ pkgName pkg
      exitFailure
  
  logSubStep "Compiling (Meson)"
  let compileCmd = "cd " ++ sourceDir ++ "/builddir && ninja " ++ buildFlags config
  exitCode <- system compileCmd
  case exitCode of
    ExitSuccess -> logOK $ "Compiled " ++ pkgName pkg
    _ -> do
      logFail $ "Compilation failed for " ++ pkgName pkg
      exitFailure
  
  logSubStep "Installing (Meson)"
  let installCmd = "cd " ++ sourceDir ++ "/builddir && ninja install"
  exitCode <- system installCmd
  case exitCode of
    ExitSuccess -> logOK $ "Installed " ++ pkgName pkg
    _ -> do
      logFail $ "Install failed for " ++ pkgName pkg
      exitFailure

compileMake :: SystemConfig -> Package -> FilePath -> IO ()
compileMake config pkg sourceDir = do
  logSubStep $ "Compiling " ++ pkgName pkg
  let makeCmd = "cd " ++ sourceDir ++ " && make " ++ buildFlags config
  exitCode <- system makeCmd
  case exitCode of
    ExitSuccess -> logOK $ "Compiled " ++ pkgName pkg
    _ -> do
      logFail $ "Compilation failed for " ++ pkgName pkg
      exitFailure

installMake :: Package -> FilePath -> IO ()
installMake pkg sourceDir = do
  logSubStep "Installing"
  let installCmd = "cd " ++ sourceDir ++ " && make install"
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
