module Page.Home where

import Lucid.Extended

html ∷ Html_
html = section_ [class_ "hero is-info"] do
  div_ [class_ "hero-body"] do
    p_ [class_ "title"] "Home page"
    p_ [class_ "subtitle"] "Welcome home!"
