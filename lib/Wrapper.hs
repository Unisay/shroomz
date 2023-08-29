module Wrapper where

import Lucid.Extended

wrapBody ∷ Html_ → Html_
wrapBody body = html_ [term "data-bs-theme" "dark"] do
  head_ do
    title_ "HTMX Demo!"
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
