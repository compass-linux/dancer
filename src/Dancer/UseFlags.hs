module Dancer.UseFlags where

import Data.List (nub, sort, isPrefixOf)
import Dancer.Types
import Dancer.Logging

parseFlag :: String -> (Bool, String)
parseFlag ('+':rest) = (True, rest)
parseFlag ('-':rest) = (False, rest)
parseFlag flag       = (True, flag)

resolveUseFlags :: SystemConfig -> Package -> [String]
resolveUseFlags config pkg =
  let supported   = pkgUseFlags pkg
      globalOn    = [f | f <- useFlags config, f `elem` supported]
      overrides   = maybe [] id (lookup (pkgName pkg) (packageUseFlags config))
      applyOne enabled spec =
        let (on, name) = parseFlag spec
        in if name `notElem` supported
             then enabled -- silently ignore unknown flag for this pkg
             else if on
                    then nub (name : enabled)
                    else filter (/= name) enabled
      resolved = foldl applyOne globalOn overrides
  in sort (nub resolved)

disabledFlags :: SystemConfig -> Package -> [String]
disabledFlags config pkg =
  let enabled = resolveUseFlags config pkg
  in [f | f <- pkgUseFlags pkg, f `notElem` enabled]

toConfigureArgs :: SystemConfig -> Package -> String
toConfigureArgs config pkg =
  let enabled  = resolveUseFlags config pkg
      disabled = disabledFlags config pkg
      enableArgs  = map (\f -> "--enable-" ++ f) enabled
      disableArgs = map (\f -> "--disable-" ++ f) disabled
  in unwords (enableArgs ++ disableArgs)

toMakeDefines :: SystemConfig -> Package -> String
toMakeDefines config pkg =
  let enabled = resolveUseFlags config pkg
  in unwords (map (\f -> "-DUSE_" ++ mapUpper f) enabled)
  where
    mapUpper = map toUpperChar
    toUpperChar c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

logResolvedFlags :: SystemConfig -> Package -> IO ()
logResolvedFlags config pkg = do
  let enabled  = resolveUseFlags config pkg
  let disabled = disabledFlags config pkg
  if not (null enabled)
    then logSubStep $ "USE flags enabled: " ++ unwords enabled
    else return ()
  if not (null disabled)
    then logSubStep $ "USE flags disabled: " ++ unwords disabled
    else return ()
