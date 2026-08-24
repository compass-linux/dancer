{-# LANGUAGE DeriveGeneric #-}
module Dancer.Types where
import GHC.Generics (Generic)

data Package = Package
  { pkgName :: String
  , pkgVersion :: String
  , pkgDeps :: [String]
  , pkgUseFlags :: [UseFlagSpec]
  , pkgSource :: PackageSource
  , buildMode :: BuildMode
  } deriving (Show, Read, Eq, Generic)

data UseFlagSpec = UseFlagSpec
  { flagName :: String
  , flagKind :: FlagKind
  } deriving (Show, Read, Eq, Generic)

data FlagKind = EnableDisable | WithWithout
  deriving (Show, Read, Eq, Generic)

data SystemConfig = SystemConfig
  { hostname :: String
  , libc :: LibC
  , coreutils :: Coreutils
  , packages :: [String]
  , useFlags :: [String]
  , packageUseFlags :: [(String, [String])]
  , buildFlags :: String
  , ldFlags :: String
  , repoSources :: [(String, String)]
  } deriving (Show, Read, Eq, Generic)

data PackageSource
  = GitSource String String
  | LocalSource String
  deriving (Show, Read, Eq, Generic)

data BuildMode
  = Autotools
  | Autoconf
  | Make
  | Meson
  deriving (Show, Read, Eq, Generic)

data LibC = Glibc | Musl deriving (Show, Read, Eq, Generic)
data Coreutils = GNU | Busybox deriving (Show, Read, Eq, Generic)

ed :: String -> UseFlagSpec
ed name = UseFlagSpec name EnableDisable

ww :: String -> UseFlagSpec
ww name = UseFlagSpec name WithWithout
