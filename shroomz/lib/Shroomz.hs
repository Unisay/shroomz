module Shroomz
  ( Shroomz
  , BodyWrapper (..)
  , new
  , renderPath
  , updatePath
  , diagnosticInfo
  , toWai
  , toWaiIO
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

data Shroomz m = Shroomz
  { components ∷ Map ComponentPath (ComponentWithState m)
  , bodyWrapper ∷ BodyWrapper
  }

newtype BodyWrapper = BodyWrapper {wrapBody ∷ Html_ → Html_}

-- | Create a new app with a single root component.
new ∷ ComponentWithState m → BodyWrapper → Shroomz m
new root bodyWrapper = Shroomz {components = componentsFromRoot root, ..}

componentsFromRoot
  ∷ ComponentWithState m
  → Map ComponentPath (ComponentWithState m)
componentsFromRoot = go Path.Root
 where
  go
    ∷ ComponentPath
    → ComponentWithState m
    → Map ComponentPath (ComponentWithState m)
  go path cws@(ComponentWithState Component {children} _) =
    Map.singleton path cws
      <> Map.foldMapWithKey (go . Path.snocSlot path) children

findComponent ∷ Shroomz m → ComponentPath → Maybe (ComponentWithState m)
findComponent app path = Map.lookup path (components app)

getComponent ∷ Applicative m ⇒ Shroomz m → ComponentPath → ComponentWithState m
getComponent app path = fromMaybe noComponent (findComponent app path)
 where
  noComponent =
    statelessComponent
      Component
        { render = \_path _state _children →
            "Component not found: " <> fromString (toString $ Path.render path)
        , update = \_state _action → pure _state
        , parseAction = const Nothing
        , children = mempty
        }

updateComponentState
  ∷ Monad m
  ⇒ Shroomz m
  → ComponentPath
  → (∀ s a. Component m s a → s → m s)
  → m (Shroomz m)
updateComponentState app path f =
  case Map.lookup path (components app) of
    Nothing → pure app
    Just cs → do
      cs' ← withComponentWithState cs \comp state →
        ComponentWithState comp <$> f comp state
      pure app {components = Map.insert path cs' (components app)}

renderPath ∷ Applicative m ⇒ Shroomz m → ComponentPath → Html_
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
  renderChild ∷ ComponentPath → ComponentWithState m → Html_
  renderChild cpath component =
    withComponentWithState component \comp state →
      render comp cpath state (renderPath app . Path.snocSlot cpath)

updatePath
  ∷ Monad m
  ⇒ Shroomz m
  → ComponentPath
  → LByteString
  → m (Shroomz m, Html_)
updatePath app path body = do
  app' ← updateComponentState app path \comp state →
    case urlDecodeForm body of
      Left _err → pure state
      Right form →
        case parseAction comp form of
          Nothing → pure state
          Just action → update comp state action
  pure (app', renderPath app' path)

diagnosticInfo ∷ Shroomz m → IO ()
diagnosticInfo Shroomz {..} = do
  putTextLn "Shroomz components:"
  for_ (Map.toList components) \(path, _) → do
    putTextLn $ Path.render path

toWai
  ∷ ∀ m
   . Monad m
  ⇒ Shroomz m
  → (∀ a. m a → IO a)
  → IO Wai.Application
toWai initialApp runM = do
  Shroomz.diagnosticInfo initialApp
  var ← newTVarIO initialApp
  pure \request withResponse → do
    app ← liftIO $ readTVarIO var
    let headers = Wai.requestHeaders request
    let path =
          NE.nonEmpty (Wai.pathInfo request)
            & maybe Path.Root Path.fromInfo
    case parseMethod (Wai.requestMethod request) of
      Right GET →
        withResponse $ respondGet app (htmxInfo headers) path
      Right POST → do
        body ← Wai.consumeRequestBodyLazy request
        (app', response) ← runM $ respondPost app path body
        atomically $ writeTVar var app'
        withResponse response
      _ → withResponse $ Wai.responseBuilder methodNotAllowed405 [] mempty

toWaiIO ∷ Shroomz IO → IO Wai.Application
toWaiIO shroomz = toWai shroomz identity

respondGet ∷ Applicative m ⇒ Shroomz m → HtmxInfo → ComponentPath → Wai.Response
respondGet app HtmxInfo {..} path = do
  let markup = Shroomz.renderPath app path
  let responseBody =
        Lucid.renderBS $
          if isHtmxRequest then markup else wrapBody (bodyWrapper app) markup
  let responseHeaders = []
  Wai.responseLBS ok200 responseHeaders responseBody

respondPost
  ∷ Monad m
  ⇒ Shroomz m
  → ComponentPath
  → LByteString
  → m (Shroomz m, Wai.Response)
respondPost app path body = do
  (updatedApp, markup) ← Shroomz.updatePath app path body
  let responseHeaders = []
      responseBody = Lucid.renderBS markup
  pure (updatedApp, Wai.responseLBS ok200 responseHeaders responseBody)

newtype HtmxInfo = HtmxInfo {isHtmxRequest ∷ Bool}

htmxInfo ∷ [Header] → HtmxInfo
htmxInfo headers = HtmxInfo {isHtmxRequest = _isHtmxRequest}
 where
  _isHtmxRequest =
    headers & any \(name, value) → name == "HX-Request" && value == "true"
