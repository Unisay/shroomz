module Shroomz.Init (initialize) where

import Data.List.NonEmpty qualified as NE
import Demo.StatefulComponent qualified as StatefulComponent
import Lucid.Extended qualified as Lucid
import Network.HTTP.Types
  ( Header
  , StdMethod (..)
  , methodNotAllowed405
  , ok200
  , parseMethod
  )
import Network.Wai qualified as Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  )
import Network.Wai.Middleware.Static (only, staticPolicy)
import Shroomz (Shroomz)
import Shroomz qualified
import Shroomz.Component.Path qualified as Path
import Wrapper (wrapBody)
import Prelude hiding (get)

initialize ∷ IO (Int, Wai.Application)
initialize = do
  var ← newTVarIO StatefulComponent.app
  pure $ (3000,) $ middleware $ \request withResponse → do
    app ← readTVarIO var
    let headers = Wai.requestHeaders request
    let path =
          NE.nonEmpty (Wai.pathInfo request)
            & maybe Path.Root Path.fromInfo
    case parseMethod (Wai.requestMethod request) of
      Right GET → withResponse $ respondGet app (htmxInfo headers) path
      Right POST → do
        body ← Wai.consumeRequestBodyLazy request
        let (app', response) = respondPost app path body
        atomically $ writeTVar var app'
        withResponse response
      _ → withResponse $ Wai.responseBuilder methodNotAllowed405 [] mempty

respondGet ∷ Shroomz → HtmxInfo → Path.ComponentPath → Wai.Response
respondGet app HtmxInfo {..} path = do
  let markup = Shroomz.renderPath app path
  let responseBody =
        Lucid.renderBS $
          if isHtmxRequest then markup else wrapBody markup
  let responseHeaders = []
  Wai.responseLBS ok200 responseHeaders responseBody

respondPost ∷ Shroomz.Shroomz → Path.ComponentPath → LByteString → (Shroomz, Wai.Response)
respondPost app path body =
  (updatedApp, Wai.responseLBS ok200 responseHeaders responseBody)
 where
  responseHeaders = []
  responseBody = Lucid.renderBS markup
  (updatedApp, markup) = Shroomz.updatePath app path body

middleware ∷ Wai.Middleware
middleware =
  logStdoutDev
    . requestSizeLimitMiddleware defaultRequestSizeLimitSettings
    . staticPolicy
      ( only
          [ ("favicon.ico", "static/images/favicon.ico")
          , ("favicon-16x16.png", "static/images/favicon-16x16.png")
          , ("favicon-32x32.png", "static/images/favicon-32x32.png")
          , ("apple-touch-icon.png", "static/images/apple-touch-icon.png")
          , ("android-chrome-192x192.png", "static/images/android-chrome-192x192.png")
          , ("android-chrome-512x512.png", "static/images/android-chrome-512x512.png")
          , ("site.webmanifest", "static/site.webmanifest")
          ]
      )

newtype HtmxInfo = HtmxInfo {isHtmxRequest ∷ Bool}

htmxInfo ∷ [Header] → HtmxInfo
htmxInfo headers = HtmxInfo {isHtmxRequest = _isHtmxRequest}
 where
  _isHtmxRequest =
    headers & any \(name, value) → name == "HX-Request" && value == "true"
