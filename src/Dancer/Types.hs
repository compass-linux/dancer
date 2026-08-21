{-# LANGUAGE DeriveGeneric #-}

module Dancer.Types where

import GHC.Generics (Generic)
import Data.Text (Text)

data Package = Package
  { pkgName :: String
  , pkgVersion :: String
  , pkgDeps :: [String]
  , pkgUseFlags :: [String]
  } deriving (Show, Generic)

data SystemConfig = SystemConfig
  { hostname :: String
  , libc :: LibC
  , coreutils :: Coreutils
  , packages :: [Package]
  , useFlags :: [String]
  , buildFlags :: String
  } deriving (Show, Generic)

data LibC = Glibc | Musl deriving (Show, Eq)
data Coreutils = GNU | Busybox deriving (Show, Eq)
