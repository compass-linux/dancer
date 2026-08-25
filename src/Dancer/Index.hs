{-# LANGUAGE DeriveGeneric #-}
module Dancer.Index where

import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName, dropExtension)
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (catMaybes, listToMaybe)
import GHC.Generics (Generic)
import Dancer.Types
import Dancer.Logging

data PackageIndex = PackageIndex
  { indexRepos :: [FilePath]
  , indexPaths :: [(String, FilePath)]  -- name -> path to package.hs file
  } deriving (Show, Generic)

buildIndex :: [FilePath] -> IO PackageIndex
buildIndex repoPaths = do
  logSubStep "Building package index"
  paths <- concat <$> mapM listPackagesInRepo repoPaths
  let byName = [(name, file) | (file, name) <- paths]
  logOK $ "Indexed " ++ show (length byName) ++ " packages"
  return $ PackageIndex repoPaths byName

searchIndex :: PackageIndex -> String -> [String]
searchIndex idx query =
  sort [name | (name, _) <- indexPaths idx, query `isInfixOf` name]

lookupPackagePath :: PackageIndex -> String -> Maybe FilePath
lookupPackagePath idx pkgName =
  listToMaybe [path | (name, path) <- indexPaths idx, name == pkgName]

resolvePackagePaths :: PackageIndex -> [String] -> [FilePath]
resolvePackagePaths idx names =
  catMaybes [lookupPackagePath idx name | name <- names]

allPackageNames :: PackageIndex -> [String]
allPackageNames = map fst . indexPaths

listPackagesInRepo :: FilePath -> IO [(FilePath, String)]
listPackagesInRepo repoPath = do
  exists <- doesDirectoryExist repoPath
  if not exists
    then return []
    else do
      cats <- listDirectory repoPath
      results <- mapM (listPackagesInCategory repoPath) cats
      return (concat results)

listPackagesInCategory :: FilePath -> String -> IO [(FilePath, String)]
listPackagesInCategory repoPath cat = do
  let catPath = repoPath </> cat
  isDir <- doesDirectoryExist catPath
  if not isDir
    then return []
    else do
      pkgs <- listDirectory catPath
      results <- mapM (\p -> do
                          let pkgFile = catPath </> p </> "Package.hs"
                          fileExists <- doesFileExist pkgFile
                          return (if fileExists
                                    then Just (pkgFile, cat ++ "/" ++ p)
                                    else Nothing)) pkgs
      return (catMaybes results)
