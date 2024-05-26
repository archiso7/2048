{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_x2048 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "x2048"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple 2048 game implementation in Haskell"
copyright :: String
copyright = "2024 archiso"
homepage :: String
homepage = ""
