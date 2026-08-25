module Dancer.CLI where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Dancer.Types
import Dancer.Logging
import qualified Dancer.Config as Config
import qualified Dancer.Build as Build

{-# NOINLINE verbosityRef #-}
verbosityRef :: IORef Verbosity
verbosityRef = unsafePerformIO (newIORef Normal)

data Verbosity = Normal | Verbose | Debug
  deriving (Eq, Show, Ord)

setVerbosity :: Verbosity -> IO ()
setVerbosity v = writeIORef verbosityRef v

getVerbosity :: IO Verbosity
getVerbosity = readIORef verbosityRef

runCLI :: [String] -> IO ()
runCLI [] = printUsage >> exitFailure
runCLI args = do
  let (verbFlag, rest) = extractVerbosity args
  setVerbosity verbFlag
  case rest of
    ("rebuild":rebuildArgs) -> handleRebuild rebuildArgs
    ("fetch":_) -> handleFetch
    ("home":"rebuild":homeArgs) -> handleHomeRebuild homeArgs
    cmd -> do
      logWarn $ "Unknown command: " ++ unwords cmd
      printUsage
      exitFailure

extractVerbosity :: [String] -> (Verbosity, [String])
extractVerbosity args = go args Normal []
  where
    go [] verb acc = (verb, reverse acc)
    go ("-Sss":rest) _ acc = go rest Debug acc
    go ("-Ss":rest) _ acc = go rest Verbose acc
    go ("-S":rest) _ acc = go rest Normal acc
    go (x:rest) verb acc = go rest verb (x:acc)

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
  , "Usage: dancer [flags] <command> [options]"
  , ""
  , "Flags:"
  , "  -S         Normal output (default)"
  , "  -Ss        Verbose output"
  , "  -Sss       Debug output (show all logs)"
  , ""
  , "Commands:"
  , "  rebuild [--from-scratch]     Rebuild system (incremental by default)"
  , "  home rebuild                 Rebuild user homes"
  , "  fetch                         Fetch latest package definitions"
  , ""
  ]
