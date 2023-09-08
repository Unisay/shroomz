module Main where

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
  waiApplication ← Shroomz.toWaiIO Launcher.app
  let port = 3000
  putTextLn $ "\nLauncher is listening on http://localhost:" <> show port
  Warp.run port $ middleware waiApplication

middleware ∷ Wai.Middleware
middleware =
  logStdoutDev . requestSizeLimitMiddleware defaultRequestSizeLimitSettings
