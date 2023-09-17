module Main where

import Control.Concurrent.Supply (newSupply)
import Launcher qualified
import Launcher.AppM (AppState (..), runAppM)
import Main.Utf8 (withUtf8)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  )
import Shroomz qualified
import Prelude hiding (get)

main ∷ IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  processIdSupply ← newSupply
  waiApplication ← runAppM AppState {processes = [], processIdSupply} do
    Shroomz.toWai =<< Launcher.initApp
  let port = 3000
  putTextLn $ "\nLauncher is listening on http://localhost:" <> show port
  Warp.run port (middleware waiApplication)

middleware ∷ Wai.Middleware
middleware =
  RequestLogger.logStdoutDev
    . requestSizeLimitMiddleware defaultRequestSizeLimitSettings
