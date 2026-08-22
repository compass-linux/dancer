module Dancer.Config where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.Process (readProcess)
import System.Exit (exitFailure)
import Data.List (isInfixOf)
import Dancer.Types
import Dancer.Logging

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/dancer/system.hs"

loadConfig :: FilePath -> IO SystemConfig
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then do
      logFail $ "Config file not found: " ++ path
      exitFailure
    else do
      logSubStep $ "Loading config from " ++ path
      configSource <- readFile path
      config <- compileAndRunConfig path configSource
      logOK "Config loaded"
      return config

compileAndRunConfig :: FilePath -> String -> IO SystemConfig
compileAndRunConfig path source = do
  logSubStep "Compiling config"
  -- this is a placeholder; real implementation would use GHC API or runhaskell
  
  -- for now, return a dummy config
  -- in production, we'd:
  -- 1. write source to temp file
  -- 2. compile with ghc or runhaskell
  -- 3. execute and capture the SystemConfig
  -- 4. parse the result
  
  return defaultSystemConfig

defaultSystemConfig :: SystemConfig
defaultSystemConfig = SystemConfig
  { hostname = "compass"
  , libc = Glibc
  , coreutils = GNU
  , packages = []
  , useFlags = []
  , buildFlags = "-j4"
  }

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
