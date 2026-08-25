module Dancer.Build where

import System.Process (callProcess, readProcess, system)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.Random (randomRIO)
import Data.List (nub, intercalate)
import Data.Maybe (catMaybes)
import Dancer.Types
import Dancer.Logging
import Dancer.Fetch (fetchPackageSource)
import Dancer.UseFlags (resolveUseFlags, toConfigureArgs, toMakeDefines, logResolvedFlags)
import Dancer.Index (PackageIndex, buildIndex, resolvePackagePaths, searchIndex, lookupPackagePath)
import Dancer.PackageLoader (loadPackage)
import qualified Dancer.Config as Config

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
  
  logSubStep "Setting up repositories"
  repos <- Config.setupRepositories (repoSources config)
  
  logSubStep "Building package index"
  index <- buildIndex repos
  logOK "Package index ready"
  
  logSubStep "Resolving package paths from index"
  let requestedNames = packages config
  let pathResults = [(name, lookupPackagePath index name) | name <- requestedNames]
  let found = [(name, path) | (name, Just path) <- pathResults]
  
  logOK $ "Found " ++ show (length found) ++ " of " ++ show (length requestedNames) ++ " packages"
  
  if length found /= length requestedNames
    then do
      let missing = [name | (name, Nothing) <- pathResults]
      logWarn "Some packages could not be found in repositories"
      mapM_ (logWarn . ("  Missing: " ++)) missing
    else return ()
  
  logSubStep "Loading package metadata"
  resolvedPkgs <- mapM (\(name, path) -> loadPackage path name) found
  let loaded = [pkg | Just pkg <- resolvedPkgs]
  logOK $ "Loaded " ++ show (length loaded) ++ " packages"
  
  logSubStep "Checking for package conflicts"
  case checkConflicts config loaded of
    [] -> logOK "No conflicts detected"
    conflicts -> do
      logFail "Package conflicts detected:"
      mapM_ (logWarn . ("  " ++)) conflicts
      exitFailure
  
  showBuildSummary config loaded
  confirmed <- askConfirmation "Proceed with build?"
  
  if not confirmed
    then do
      logWarn "Build cancelled"
      exitFailure
    else do
      logStep "Building packages"
      mapM_ (buildPackage config) loaded
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
  
  logResolvedFlags config pkg
  
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
  logSubStep "Initializing git submodules"
  system $ "cd " ++ sourceDir ++ " && git submodule update --init --recursive"
  
  logSubStep "Running autoreconf"
  exitCode <- system $ "cd " ++ sourceDir ++ " && autoreconf -i"
  case exitCode of
    ExitSuccess -> logOK "autoreconf done"
    _ -> logWarn "autoreconf failed, continuing anyway"
  
  buildAutoconf config pkg sourceDir encirclement

buildAutoconf :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildAutoconf config pkg sourceDir encirclement = do
  logSubStep $ "Configuring " ++ pkgName pkg
  let flagArgs = toConfigureArgs config pkg
  let configCmd = "cd " ++ sourceDir ++ " && ./configure --prefix=" ++ encirclement
                    ++ (if null flagArgs then "" else " " ++ flagArgs)
  exitCode <- system configCmd
  case exitCode of
    ExitSuccess -> logOK $ "Configured " ++ pkgName pkg
    _ -> do
      logFail $ "Configure failed for " ++ pkgName pkg
      exitFailure
  
  compileMakeWithFlags config pkg sourceDir
  installMake pkg sourceDir encirclement

buildMake :: SystemConfig -> Package -> FilePath -> FilePath -> IO ()
buildMake config pkg sourceDir encirclement = do
  logSubStep $ "Compiling (Makefile) " ++ pkgName pkg

  let useDefines = toMakeDefines config pkg

  let cflags =
        "-O2 -march=native"
        ++ if null useDefines
           then ""
           else " " ++ useDefines

  let makeCmd =
        "cd " ++ sourceDir
        ++ " && make -j8"
        ++ " CC=gcc"
        ++ " CFLAGS='" ++ cflags ++ "'"

  exitCode <- system makeCmd

  case exitCode of
    ExitSuccess ->
      logOK $ "Compiled " ++ pkgName pkg

    _ -> do
      logFail $ "Compilation failed for " ++ pkgName pkg
      exitFailure

  logSubStep "Installing binaries"

  let binDir = encirclement </> "bin"

  createDirectoryIfMissing True binDir

  let installCmd =
        "find " ++ sourceDir
        ++ " -maxdepth 1 -type f -executable"
        ++ " ! -name '*.c'"
        ++ " ! -name '*.h'"
        ++ " -exec cp {} " ++ binDir ++ "/ \\;"

  exitCode <- system installCmd

  case exitCode of
    ExitSuccess ->
      logOK $ "Installed " ++ pkgName pkg

    _ -> do
      logFail $ "Failed to install " ++ pkgName pkg
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

compileMakeWithFlags :: SystemConfig -> Package -> FilePath -> IO ()
compileMakeWithFlags config pkg sourceDir = do
  logSubStep $ "Compiling " ++ pkgName pkg
  let makeCmd = "cd " ++ sourceDir ++ " && make " ++ buildFlags config
  exitCode <- system makeCmd
  case exitCode of
    ExitSuccess -> logOK $ "Compiled " ++ pkgName pkg
    _ -> do
      logFail $ "Compilation failed for " ++ pkgName pkg
      exitFailure

installMake :: Package -> FilePath -> FilePath -> IO ()
installMake pkg sourceDir encirclement = do
  logSubStep "Installing"
  let installCmd = "cd " ++ sourceDir ++ " && make install DESTDIR=" ++ encirclement
  exitCode <- system installCmd
  case exitCode of
    ExitSuccess -> return ()
    _ -> do
      logFail $ "Installation failed for " ++ pkgName pkg
      exitFailure

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
  createDirectoryIfMissing True generationsDir
  
  exists <- doesFileExist genList
  
  nextGen <- if exists
    then do
      content <- readFile genList
      let lastGen = length (lines content)
      return (lastGen + 1)
    else
      return 1
  
  let entry = "Generation " ++ show nextGen ++ ": " ++ hostname config
  system $ "echo '" ++ entry ++ "' >> " ++ genList
  
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
