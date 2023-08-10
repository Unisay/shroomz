module Layout where

import Lucid.Extended
import Lucid.Hx (hxGet_, hxTarget_)

html ∷ Html_ → Html_
html page = html_ [term "data-bs-theme" "dark"] do
  head_ do
    title_ "HTMX Demo"
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    script_
      [ src_ "https://unpkg.com/htmx.org@1.9.4"
      , integrity_
          "sha384-zUfuhFKKZCbHTY6aRR46gxiqszMk\
          \5tcHjsVFxnUo8VMus4kHGVdIYVbOYYNlKmHV"
      , crossorigin_ "anonymous"
      ]
      none_
    link_
      [ rel_ "stylesheet"
      , href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      ]

  nav_ [class_ "level"] do
    let item link = a_ [class_ "link is-info", hxGet_ link, hxTarget_ "#page"]
    p_ [class_ "level-item has-text-centered"] do
      item "/page/home" "Home"
    p_ [class_ "level-item has-text-centered"] do
      item "/page/docs" "Documentation"
    p_ [class_ "level-item has-text-centered"] do
      img_
        [ src_ "static/images/logo.png"
        , alt_ "logo"
        , width_ "64"
        , height_ "64"
        ]
    p_ [class_ "level-item has-text-centered"] do
      a_ [class_ "link is-info", href_ "/"] "Reservations"
    p_ [class_ "level-item has-text-centered"] do
      a_ [class_ "link is-info", href_ "/"] "Contact"

  div_ [id_ "page"] page
