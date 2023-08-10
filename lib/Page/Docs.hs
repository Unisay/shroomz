module Page.Docs where

import Lucid.Extended

html ∷ Html_
html = section_ [class_ "hero is-success"] do
  div_ [class_ "hero-body"] do
    p_ [class_ "title"] "Docs page"
    p_ [class_ "subtitle"] "Documentation lives here!"
