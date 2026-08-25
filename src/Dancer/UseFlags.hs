module Dancer.UseFlags where

import Data.List (nub, sort)
import Dancer.Types
import Dancer.Logging

parseFlag :: String -> (Bool, String)
parseFlag ('+':rest) = (True, rest)
parseFlag ('-':rest) = (False, rest)
parseFlag flag       = (True, flag)

-- | resolve the effective, enabled USE flags for a single package.
--
-- order of precedence (lowest to highest):
--   1. global useFlags in SystemConfig, restricted to flags the package
--      actually declares support for (pkgUseFlags)
--   2. per-package overrides from packageUseFlags, matched by pkgName
resolveUseFlags :: SystemConfig -> Package -> [String]
resolveUseFlags config pkg =
  let supported   = map flagName (pkgUseFlags pkg)
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
      
  in [flagName f | f <- pkgUseFlags pkg, flagName f `notElem` enabled]

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
