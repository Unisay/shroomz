module Wrapper where

import Lucid.Extended

wrapBody ∷ Html_ → Html_
wrapBody body = html_ [term "data-bs-theme" "dark"] do
  head_ do
    title_ "Shroomz Demo 🍄"
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
    script_
      [ src_ "https://unpkg.com/hyperscript.org@0.9.11"
      ]
      none_
    link_
      [ rel_ "manifest"
      , href_ "/site.webmanifest"
      ]
    link_
      [ rel_ "stylesheet"
      , href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      ]
    link_
      [ rel_ "apple-touch-icon"
      , sizes_ "180x180"
      , href_ "/apple-touch-icon.png"
      ]
    link_
      [ rel_ "icon"
      , type_ "image/png"
      , sizes_ "32x32"
      , href_ "/favicon-32x32.png"
      ]
    link_
      [ rel_ "icon"
      , type_ "image/png"
      , sizes_ "16x16"
      , href_ "/favicon-16x16.png"
      ]
  body_ do
    body
    script_
      [ type_ "module"
      , src_ "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.esm.js"
      ]
      none_
    script_
      [ src_ "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.js"
      ]
      none_
