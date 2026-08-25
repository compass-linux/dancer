module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Dancer.Types
import Dancer.Logging
import qualified Dancer.Config as Config

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["search", query] -> handleSearch query
    ["uses", pkg]      -> handleUses pkg
    _                  -> printUsage >> exitFailure

handleSearch :: String -> IO ()
handleSearch query = do
  repos <- Config.listLocalRepos
  if null repos
    then do
      logWarn "No local repositories found -- run 'dancer rebuild' at least once first"
      exitFailure
    else do
      matches <- Config.searchPackages query repos
      case matches of
        [] -> do
          putStrLn $ "No packages found matching \"" ++ query ++ "\""
          exitFailure
        _ -> do
          putStrLn $ "Found " ++ show (length matches) ++ " package(s):"
          mapM_ (\m -> putStrLn ("  " ++ m)) matches
          exitSuccess

handleUses :: String -> IO ()
handleUses pkgArg = do
  repos <- Config.listLocalRepos
  if null repos
    then do
      logWarn "No local repositories found -- run 'dancer rebuild' at least once first"
      exitFailure
    else do
      result <- Config.resolvePackageByName pkgArg repos
      case result of
        Nothing -> do
          putStrLn $ "Package not found: " ++ pkgArg
          exitFailure
        Just pkg -> do
          putStrLn $ pkgName pkg ++ " (" ++ pkgVersion pkg ++ ")"
          if null (pkgUseFlags pkg)
            then putStrLn "  No USE flags declared"
            else do
              putStrLn "  USE flags:"
              mapM_ (\(UseFlagSpec name kind) -> putStrLn ("    " ++ name)) (pkgUseFlags pkg)
          exitSuccess

printUsage :: IO ()
printUsage = putStr $ unlines
  [ "dquery: Compass Linux package lookup"
  , ""
  , "Usage: dquery <command> [args]"
  , ""
  , "Commands:"
  , "  search <name>    Search local package repos for a name/substring"
  , "  uses <cat/pkg>   Show USE flags declared by a package"
  , ""
  ]
