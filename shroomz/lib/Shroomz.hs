module Shroomz
  ( Shroomz
  , BodyWrapper (..)
  , new
  , renderPath
  , updatePath
  , diagnosticInfo
  , toWai
  ) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Lucid.Extended (Html_)
import Lucid.Extended qualified as Lucid
import Network.HTTP.Types
  ( Header
  , StdMethod (GET, POST)
  , methodNotAllowed405
  , ok200
  , parseMethod
  )
import Network.Wai qualified as Wai
import Shroomz.Component
  ( Component (..)
  , ComponentWithState (..)
  , statelessComponent
  , withComponentWithState
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Path qualified as Path
import Web.FormUrlEncoded (urlDecodeForm)
import Prelude hiding (get, state)

data Shroomz = Shroomz
  { components ∷ Map ComponentPath ComponentWithState
  , bodyWrapper ∷ BodyWrapper
  }

newtype BodyWrapper = BodyWrapper {wrapBody ∷ Html_ → Html_}

-- | Create a new app with a single root component.
new ∷ ComponentWithState → BodyWrapper → Shroomz
new root bodyWrapper = Shroomz {components = componentsFromRoot root, ..}

componentsFromRoot ∷ ComponentWithState → Map ComponentPath ComponentWithState
componentsFromRoot = go Path.Root
 where
  go
    ∷ ComponentPath
    → ComponentWithState
    → Map ComponentPath ComponentWithState
  go path cws@(ComponentWithState Component {children} _) =
    Map.singleton path cws
      <> Map.foldMapWithKey (go . Path.snocSlot path) children

findComponent ∷ Shroomz → ComponentPath → Maybe ComponentWithState
findComponent app path = Map.lookup path (components app)

getComponent ∷ Shroomz → ComponentPath → ComponentWithState
getComponent app path = fromMaybe noComponent (findComponent app path)
 where
  noComponent =
    statelessComponent
      Component
        { render = \_path _state _children →
            "Component not found: " <> fromString (toString $ Path.render path)
        , update = const
        , parseAction = const Nothing
        , children = mempty
        }

updateComponentState
  ∷ Shroomz → ComponentPath → (∀ s m. Component s m → s → s) → Shroomz
updateComponentState app path f =
  app {components = Map.adjust updateComponentWithState path (components app)}
 where
  updateComponentWithState cws = withComponentWithState cws \comp state →
    ComponentWithState comp (f comp state)

renderPath ∷ Shroomz → ComponentPath → Html_
renderPath app path =
  withComponentWithState (getComponent app path) \comp state → do
    render comp path state \slot → do
      let cpath = Path.snocSlot path slot
      case Map.lookup cpath (components app) of
        Just child → renderChild cpath child
        Nothing →
          fromString . toString $
            "Component not found: " <> Path.render cpath
 where
  renderChild ∷ ComponentPath → ComponentWithState → Html_
  renderChild cpath component =
    withComponentWithState component \comp state →
      render comp cpath state (renderPath app . Path.snocSlot cpath)

updatePath ∷ Shroomz → ComponentPath → LByteString → (Shroomz, Html_)
updatePath app path body = (app', renderPath app' path)
 where
  app' = updateComponentState app path \comp state →
    case urlDecodeForm body of
      Left _err → state
      Right form →
        case parseAction comp form of
          Nothing → state
          Just action → update comp state action

diagnosticInfo ∷ MonadIO m ⇒ Shroomz → m ()
diagnosticInfo Shroomz {..} = liftIO do
  putTextLn "Shroomz components:"
  for_ (Map.toList components) \(path, _) → do
    putTextLn $ Path.render path

toWai ∷ Shroomz → IO (Int, Wai.Application)
toWai initialApp = do
  Shroomz.diagnosticInfo initialApp
  var ← newTVarIO initialApp
  pure $ (3000,) $ \request withResponse → do
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

respondGet ∷ Shroomz → HtmxInfo → ComponentPath → Wai.Response
respondGet app HtmxInfo {..} path = do
  let markup = Shroomz.renderPath app path
  let responseBody =
        Lucid.renderBS $
          if isHtmxRequest then markup else wrapBody (bodyWrapper app) markup
  let responseHeaders = []
  Wai.responseLBS ok200 responseHeaders responseBody

respondPost ∷ Shroomz → ComponentPath → LByteString → (Shroomz, Wai.Response)
respondPost app path body =
  (updatedApp, Wai.responseLBS ok200 responseHeaders responseBody)
 where
  responseHeaders = []
  responseBody = Lucid.renderBS markup
  (updatedApp, markup) = Shroomz.updatePath app path body

newtype HtmxInfo = HtmxInfo {isHtmxRequest ∷ Bool}

htmxInfo ∷ [Header] → HtmxInfo
htmxInfo headers = HtmxInfo {isHtmxRequest = _isHtmxRequest}
 where
  _isHtmxRequest =
    headers & any \(name, value) → name == "HX-Request" && value == "true"
