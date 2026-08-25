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
  , flagOption :: String
  , flagKind :: FlagKind
  } deriving (Show, Read, Eq, Generic)

data FlagKind
  = EnableDisable
  | WithWithout
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

data LibC
  = Glibc
  | Musl
  deriving (Show, Read, Eq, Generic)

data Coreutils
  = GNU
  | Busybox
  deriving (Show, Read, Eq, Generic)

ed :: String -> String -> UseFlagSpec
ed name option = UseFlagSpec name option EnableDisable

ww :: String -> String -> UseFlagSpec
ww name option = UseFlagSpec name option WithWithout
