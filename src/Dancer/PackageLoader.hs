module Dancer.PackageLoader where

import System.Process (system)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeFileName, dropExtension)
import Data.List (isPrefixOf)
import Dancer.Types
import Dancer.Logging

loadPackage :: FilePath -> String -> IO (Maybe Package)
loadPackage pkgFile pkgName = do
  logProgress $ "Compiling " ++ pkgName ++ " package definition"
  
  let moduleName = takeFileName (dropExtension pkgFile)
  let pkgDir = takeDirectory pkgFile
  
  result <- compileAndRun pkgFile moduleName pkgDir "pkg"
  case result of
    Just pkg -> do
      logOK $ "Loaded " ++ pkgName
      return (Just pkg)
    Nothing -> do
      logWarn $ "Failed to load " ++ pkgName
      return Nothing

compileAndRun :: FilePath -> String -> FilePath -> String -> IO (Maybe Package)
compileAndRun pkgFile moduleName pkgDir binding = do
  let buildDir = "/tmp/dancer-pkg-compile"
  let loaderFile = buildDir </> "Loader.hs"
  let binaryPath = buildDir </> "loader"
  let outputFile = buildDir </> "out.txt"
  
  createDirectoryIfMissing True buildDir
  
  writeFile loaderFile $ unlines
    [ "import Dancer.Types"
    , "import " ++ moduleName
    , "main = print " ++ binding
    ]
  
  let compileCmd = unwords
        [ "ghc -v0"
        , "-i/usr/lib/dancer/src"
        , "-i" ++ pkgDir
        , "-outputdir " ++ buildDir
        , "-o " ++ binaryPath
        , loaderFile
        -- , "2>/dev/null"
        ]
  
  exitCode <- system compileCmd
  case exitCode of
    ExitSuccess -> do
      let runCmd = binaryPath ++ " > " ++ outputFile ++ " 2>/dev/null"
      exitCode2 <- system runCmd
      case exitCode2 of
        ExitSuccess -> do
          output <- readFile outputFile
          case reads output of
            [(pkg, _)] -> return (Just pkg)
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

takeDirectory :: FilePath -> FilePath
takeDirectory path =
  let rev = reverse path
      afterSlash = dropWhile (/= '/') rev
  in reverse (drop 1 afterSlash)
