module Dancer.Config where

import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Process (readProcess, system)
import System.Exit (exitFailure, ExitCode(..))
import Data.List (isInfixOf, intercalate)
import Data.Maybe (catMaybes, listToMaybe)
import Dancer.Types
import Dancer.Logging

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/dancer/system.hs"

reposDir :: FilePath
reposDir = "/var/lib/dancer/repos"

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

setupRepositories :: [(String, String)] -> IO [FilePath]
setupRepositories repos = do
  logSubStep "Setting up repositories"
  createDirectoryIfMissing True reposDir
  repoPaths <- mapM (uncurry (fetchRepo reposDir)) repos
  logOK $ "Repositories ready"
  return repoPaths

fetchRepo :: FilePath -> String -> String -> IO FilePath
fetchRepo baseDir url branch = do
  let repoName = getRepoName url
  let repoPath = baseDir </> repoName
  
  exists <- doesDirectoryExist repoPath
  
  if exists
    then do
      logProgress $ "Updating repository " ++ repoName
      system $ "cd " ++ repoPath ++ " && git pull origin " ++ branch
      return repoPath
    else do
      logProgress $ "Cloning repository " ++ repoName
      exitCode <- system $ "git clone --branch " ++ branch ++ " " ++ url ++ " " ++ repoPath
      case exitCode of
        ExitSuccess -> do
          logOK $ "Cloned " ++ repoName
          return repoPath
        _ -> do
          logFail $ "Failed to clone " ++ url
          error $ "Git clone failed for " ++ url

getRepoName :: String -> String
getRepoName url =
  let base = takeFileName (dropEnd 4 url)
  in if null base then "pkgs" else base
  where
    dropEnd n xs = take (length xs - n) xs

resolvePackagesByNameFromRepos :: [String] -> [FilePath] -> IO [Package]
resolvePackagesByNameFromRepos pkgNames repos = do
  packages <- mapM (`resolvePackageByName` repos) pkgNames
  let resolved = catMaybes packages
  return resolved

resolvePackageByName :: String -> [FilePath] -> IO (Maybe Package)
resolvePackageByName pkgName repoPaths = do
  results <- mapM (tryLoadPackage pkgName) repoPaths
  case listToMaybe [p | Just p <- results] of
    Just pkg -> return (Just pkg)
    Nothing -> do
      logWarn $ "Package not found in any repository: " ++ pkgName
      return Nothing

tryLoadPackage :: String -> FilePath -> IO (Maybe Package)
tryLoadPackage pkgName repoPath = do
  let parts = splitOn "/" pkgName
  case parts of
    [cat, pkg] -> do
      let pkgFile = repoPath </> cat </> pkg </> "package.hs"
      exists <- doesFileExist pkgFile
      if exists
        then loadPackageDefinition pkgFile pkgName
        else return Nothing
    _ -> return Nothing

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
  putStrLn $ "  Repositories: " ++ show (length (repoSources config))
