{-# LANGUAGE DeriveGeneric #-}
module Dancer.Types where
import GHC.Generics (Generic)

data Package = Package
  { pkgName :: String
  , pkgVersion :: String
  , pkgDeps :: [String]
  , pkgUseFlags :: [String]
  , pkgSource :: PackageSource
  , buildMode :: BuildMode
  } deriving (Show, Read, Eq, Generic)

data BuildMode
  = Autotools
  | Autoconf
  | Make
  | Meson
  deriving (Show, Read, Eq, Generic)

data SystemConfig = SystemConfig
  { hostname :: String
  , libc :: LibC
  , coreutils :: Coreutils
  , packages :: [Package]
  , useFlags :: [String]
  , buildFlags :: String
  , ldFlags :: String
  } deriving (Show, Read, Eq, Generic)

data PackageSource
  = GitSource String String
  | LocalSource String
  deriving (Show, Read, Eq, Generic)

data LibC = Glibc | Musl deriving (Show, Read, Eq, Generic)
data Coreutils = GNU | Busybox deriving (Show, Read, Eq, Generic)
