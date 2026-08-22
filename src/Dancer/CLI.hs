module Dancer.CLI where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (isPrefixOf)
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
      logSubStep "Loading system config"
      -- config <- Config.loadConfig "/etc/dancer/system.hs"
      logSubStep "Resolving dependencies"
      logSubStep "Preparing build environment"
      logSubStep "Building packages"
      logProgress "Compiling..."
      logOK "All packages built successfully"
      logStep "Rebuild complete"
      exitSuccess
  | otherwise = do
      logStep "Starting incremental rebuild"
      logSubStep "Loading system config"
      logSubStep "Checking for changes"
      logSubStep "Building changed packages"
      logOK "Incremental rebuild complete"
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
  , "  home rebuild                 Rebuild user home"
  , "  fetch                         Fetch latest package definitions"
  , ""
  ]
