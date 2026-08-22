module Dancer.Config where

import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, removeFile, listDirectory)
import System.Process (readProcess, system)
import System.Exit (exitFailure, ExitCode(..))
import Data.List (isInfixOf, intercalate)
import Data.Maybe (catMaybes)
import Dancer.Types
import Dancer.Logging

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/dancer/system.hs"

repoPath :: FilePath
repoPath = "/var/lib/dancer/db"

scratchDir :: FilePath
scratchDir = "/tmp/dancer-pkg-build"

loadConfig :: FilePath -> IO SystemConfig
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then do
      logFail $ "Config file not found: " ++ path
      exitFailure
    else do
      logSubStep $ "Loading config from " ++ path
      config <- compileAndRunConfig path
      logOK "Config loaded"
      return config

compileAndRunConfig :: FilePath -> IO SystemConfig
compileAndRunConfig path = do
  logSubStep "Compiling config with GHC"

  system $ "mkdir -p " ++ scratchDir
  system $ "cp " ++ path ++ " " ++ scratchDir ++ "/system.hs"

  let scratchSource = scratchDir ++ "/system.hs"
      binaryPath = scratchDir ++ "/system.bin"

  exitCode <- system $ "ghc -v0 -i/home/confucius/compass/dancer/src -outputdir " ++ scratchDir ++ " -o " ++ binaryPath ++ " " ++ scratchSource

  case exitCode of
    ExitSuccess -> do
      logOK "Config compiled"
      logSubStep "Executing config"
      output <- readProcess binaryPath [] ""
      cleanup
      case reads output of
        [(config, _)] -> do
          logOK "Config parsed"
          return config
        _ -> do
          logFail "Failed to parse config output"
          exitFailure
    _ -> do
      logFail $ "GHC compilation failed for " ++ path
      exitFailure

cleanup :: IO ()
cleanup = do
  system $ "rm -rf " ++ scratchDir
  return ()

resolvePackagesByName :: [String] -> IO [Package]
resolvePackagesByName pkgNames = do
  logSubStep "Resolving packages from repository"
  packages <- mapM resolvePackageByName pkgNames
  let resolved = catMaybes packages
  logOK $ "Resolved " ++ show (length resolved) ++ " packages"
  return resolved

resolvePackageByName :: String -> IO (Maybe Package)
resolvePackageByName pkgName = do
  let parts = splitOn "/" pkgName
  case parts of
    [cat, pkg] -> do
      let pkgFile = repoPath </> cat </> pkg </> "package.hs"
      exists <- doesFileExist pkgFile
      if not exists
        then do
          logWarn $ "Package not found: " ++ pkgName
          return Nothing
        else loadPackageDefinition pkgFile pkgName
    _ -> do
      logWarn $ "Invalid package name: " ++ pkgName
      return Nothing

loadPackageDefinition :: FilePath -> String -> IO (Maybe Package)
loadPackageDefinition pkgFile pkgName = do
  logProgress $ "Loading " ++ pkgName
  
  let moduleFile = scratchDir ++ "/LoadPkg.hs"
  let binaryPath = scratchDir ++ "/loadpkg.bin"
  
  system $ "mkdir -p " ++ scratchDir
  system $ "cp " ++ pkgFile ++ " " ++ moduleFile
  
  exitCode <- system $ "ghc -v0 -i/home/confucius/compass/dancer/src -outputdir " ++ scratchDir ++ " -e 'print pkg' -o " ++ binaryPath ++ " " ++ moduleFile ++ " 2>/dev/null"
  
  case exitCode of
    ExitSuccess -> do
      output <- readProcess binaryPath [] ""
      case reads output of
        [(package, _)] -> do
          logOK $ "Loaded " ++ pkgName
          return (Just package)
        _ -> do
          logWarn $ "Failed to parse package from " ++ pkgName
          return Nothing
    _ -> do
      logWarn $ "Failed to compile " ++ pkgName
      return Nothing

splitOn :: String -> String -> [String]
splitOn delim str = case break (== head delim) str of
  (a, []) -> [a]
  (a, _:b) -> a : splitOn delim b

validateConfig :: SystemConfig -> Either String ()
validateConfig config
  | null (hostname config) = Left "hostname cannot be empty"
  | null (buildFlags config) = Left "buildFlags cannot be empty"
  | otherwise = Right ()

printConfig :: SystemConfig -> IO ()
printConfig config = do
  putStrLn "System Configuration:"
  putStrLn $ "  Hostname: " ++ hostname config
  putStrLn $ "  LibC: " ++ show (libc config)
  putStrLn $ "  Coreutils: " ++ show (coreutils config)
  putStrLn $ "  Build Flags: " ++ buildFlags config
  putStrLn $ "  Packages: " ++ show (length (packages config))
  putStrLn $ "  Use Flags: " ++ show (useFlags config)
