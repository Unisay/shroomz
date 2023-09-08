module Main where

import Demo.Index qualified as Index
import Main.Utf8 (withUtf8)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  )
import Network.Wai.Middleware.Static (only, staticPolicy)
import Relude.Extra.Tuple (toSnd)
import Shroomz qualified
import Prelude hiding (get)

main ∷ IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  (port, waiApplication) ← Shroomz.toWai Index.app
  putTextLn $ "\nShroomz Showcase is listening on http://localhost:" <> show port
  Warp.run port $ middleware waiApplication

middleware ∷ Wai.Middleware
middleware =
  logStdoutDev
    . requestSizeLimitMiddleware defaultRequestSizeLimitSettings
    . staticPolicy
      ( let static = ("static/" <>)
            image = toSnd (static "images/" <>)
         in only
              [ image "favicon.ico"
              , image "favicon-16x16.png"
              , image "favicon-32x32.png"
              , image "apple-touch-icon.png"
              , image "android-chrome-192x192.png"
              , image "android-chrome-512x512.png"
              , ("site.webmanifest", static "site.webmanifest")
              ]
      )
