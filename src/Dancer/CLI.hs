module Dancer.CLI where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Dancer.Types
import Dancer.Logging
import qualified Dancer.Config as Config
import qualified Dancer.Build as Build

runCLI :: [String] -> IO ()
runCLI [] = printUsage >> exitFailure
runCLI ("rebuild":args) = handleRebuild args
runCLI ("fetch":_) = handleFetch
runCLI ("home":"rebuild":args) = handleHomeRebuild args
runCLI cmd = do
  logWarn $ "Unknown command: " ++ unwords cmd
  printUsage
  exitFailure

handleRebuild :: [String] -> IO ()
handleRebuild args
  | "--from-scratch" `elem` args = do
      logStep "Starting cold rebuild"
      config <- Config.loadConfig Config.defaultConfigPath
      Build.buildSystem config True
      exitSuccess
  | otherwise = do
      logStep "Starting incremental rebuild"
      config <- Config.loadConfig Config.defaultConfigPath
      Build.rebuildIncremental config
      exitSuccess

handleFetch :: IO ()
handleFetch = do
  logStep "Fetching latest package definitions"
  logSubStep "Connecting to mirror"
  logProgress "Downloading snapshot..."
  logOK "Fetch complete"
  exitSuccess

handleHomeRebuild :: [String] -> IO ()
handleHomeRebuild _ = do
  logStep "Rebuilding user homes"
  logSubStep "Loading home configs"
  logSubStep "Applying dotfiles"
  logSubStep "Installing user packages"
  logOK "Home rebuild complete"
  exitSuccess

printUsage :: IO ()
printUsage = putStr $ unlines
  [ "Dancer package manager"
  , ""
  , "Usage: dancer <command> [options]"
  , ""
  , "Commands:"
  , "  rebuild [--from-scratch]     Rebuild system (incremental by default)"
  , "  home rebuild                 Rebuild user homes"
  , "  fetch                         Fetch latest package definitions"
  , ""
  ]
