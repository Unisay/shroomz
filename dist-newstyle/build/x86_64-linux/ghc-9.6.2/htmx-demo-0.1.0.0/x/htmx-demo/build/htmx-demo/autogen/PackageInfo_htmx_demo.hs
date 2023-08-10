{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_htmx_demo (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "htmx_demo"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "HTMX Demo"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
