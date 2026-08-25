module Dancer.UseFlags where

import Data.List (sortOn)
import Dancer.Types
import Dancer.Logging

parseFlag :: String -> (Bool, String)
parseFlag ('+':rest) = (True, rest)
parseFlag ('-':rest) = (False, rest)
parseFlag flag = (True, flag)

resolveUseFlags :: SystemConfig -> Package -> [UseFlagSpec]
resolveUseFlags config pkg =
  let supported = pkgUseFlags pkg
      global = useFlags config
      overrides =
        maybe [] id
          (lookup (pkgName pkg) (packageUseFlags config))

      enabled = foldl applyFlag [] global

  in foldl applyFlag enabled overrides

  where
    applyFlag :: [UseFlagSpec] -> String -> [UseFlagSpec]
    applyFlag current spec =
      let (enabledFlag, name) = parseFlag spec
      in case findFlag name (pkgUseFlags pkg) of
           Nothing ->
             current

           Just flag ->
             if enabledFlag
               then addFlag flag current
               else removeFlag name current

findFlag :: String -> [UseFlagSpec] -> Maybe UseFlagSpec
findFlag name [] = Nothing
findFlag name (flag:flags)
  | flagName flag == name = Just flag
  | otherwise = findFlag name flags

addFlag :: UseFlagSpec -> [UseFlagSpec] -> [UseFlagSpec]
addFlag flag flags =
  flag : filter ((/= flagName flag) . flagName) flags

removeFlag :: String -> [UseFlagSpec] -> [UseFlagSpec]
removeFlag name =
  filter ((/= name) . flagName)

sortFlags :: [UseFlagSpec] -> [UseFlagSpec]
sortFlags =
  sortOn flagName

enabledOptions :: SystemConfig -> Package -> [String]
enabledOptions config pkg =
  map flagArgument (sortFlags (resolveUseFlags config pkg))
  where
    flagArgument flag =
      case flagKind flag of
        EnableDisable ->
          "--enable-" ++ flagOption flag

        WithWithout ->
          "--with-" ++ flagOption flag

disabledOptions :: SystemConfig -> Package -> [String]
disabledOptions config pkg =
  let supported = pkgUseFlags pkg
      enabled = resolveUseFlags config pkg
      enabledNames = map flagName enabled
  in map disabledArgument
       [flag | flag <- supported, flagName flag `notElem` enabledNames]
  where
    disabledArgument flag =
      case flagKind flag of
        EnableDisable ->
          "--disable-" ++ flagOption flag

        WithWithout ->
          "--without-" ++ flagOption flag

toConfigureArgs :: SystemConfig -> Package -> String
toConfigureArgs config pkg =
  unwords $
    enabledOptions config pkg
    ++ disabledOptions config pkg

toMakeDefines :: SystemConfig -> Package -> String
toMakeDefines config pkg =
  unwords $
    map makeDefine (sortFlags (resolveUseFlags config pkg))
  where
    makeDefine flag =
      "-DUSE_" ++ mapUpper (flagName flag)

    mapUpper =
      map toUpperChar

    toUpperChar c
      | c >= 'a' && c <= 'z' =
          toEnum (fromEnum c - 32)
      | otherwise =
          c

logResolvedFlags :: SystemConfig -> Package -> IO ()
logResolvedFlags config pkg = do
  let enabled = sortFlags (resolveUseFlags config pkg)
      disabled =
        let supported = pkgUseFlags pkg
            enabledNames = map flagName enabled
        in [flag | flag <- supported, flagName flag `notElem` enabledNames]

  if null enabled
    then return ()
    else do
      logSubStep "USE flags enabled:"
      mapM_ logEnabled enabled

  if null disabled
    then return ()
    else do
      logSubStep "USE flags disabled:"
      mapM_ logDisabled disabled

  where
    logEnabled flag =
      logSubStep $
        "  " ++ flagName flag
        ++ " -> "
        ++ flagOption flag

    logDisabled flag =
      logSubStep $
        "  " ++ flagName flag
        ++ " -> disabled"
