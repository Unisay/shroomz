module Shroomz
  ( module Shroomz.Types
  , BodyWrapper (..)
  , initApp
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
  , ComponentData (..)
  , ComponentWithData (..)
  , withComponentWithData
  , withSomeComponent
  )
import Shroomz.Component qualified as Component
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Path qualified as Path
import Shroomz.Types
import UnliftIO (MonadUnliftIO, withRunInIO)
import Web.FormUrlEncoded (urlDecodeForm)
import Prelude hiding (get, state)

-- | Create a new app with a single root component.
initApp
  ∷ ∀ m state action
   . Monad m
  ⇒ Component m state action
  → BodyWrapper
  → m (Shroomz m)
initApp rootComponent bodyWrapper = do
  components ← initFromRoot Path.Root rootComponent
  pure Shroomz {components = Map.fromList components, bodyWrapper}
 where
  initFromRoot
    ∷ ∀ s a
     . ComponentPath
    → Component m s a
    → m [(ComponentPath, ComponentWithData m)]
  initFromRoot path c@Component {initialise} = do
    d@ComponentData {children} ← initialise
    ((path, ComponentWithData c d) :) . join
      <$> forM (Map.toList children) \(slot, child) →
        withSomeComponent child $ initFromRoot (Path.snocSlot path slot)

findComponent ∷ Shroomz m → ComponentPath → Maybe (ComponentWithData m)
findComponent app path = Map.lookup path (components app)

getComponent ∷ Applicative m ⇒ Shroomz m → ComponentPath → ComponentWithData m
getComponent app path = fromMaybe noComponent (findComponent app path)
 where
  noData = ComponentData {children = mempty, userState = ()}
  noComponent =
    ComponentWithData
      Component
        { initialise = pure noData
        , render = \_path _state _children →
            "Component not found: " <> fromString (toString $ Path.render path)
        , update = \_state _action → pure _state
        , parseAction = const Nothing
        }
      noData

renderPath ∷ Applicative m ⇒ Shroomz m → ComponentPath → Html_
renderPath app path =
  withComponentWithData (getComponent app path) \c d → do
    Component.render c path (userState d) \slot → do
      let cpath = Path.snocSlot path slot
      case Map.lookup cpath (components app) of
        Just child → renderChild cpath child
        Nothing →
          fromString . toString $
            "Component not found: " <> Path.render cpath
 where
  renderChild ∷ ComponentPath → ComponentWithData m → Html_
  renderChild cpath component = withComponentWithData component \comp d →
    Component.render
      comp
      cpath
      (userState d)
      (renderPath app . Path.snocSlot cpath)

diagnosticInfo ∷ Shroomz m → IO ()
diagnosticInfo Shroomz {..} = do
  putTextLn "Shroomz components:"
  for_ (Map.toList components) \(path, _) → do
    putTextLn $ Path.render path

toWai ∷ ∀ m. MonadUnliftIO m ⇒ Shroomz m → m Wai.Application
toWai initialApp = do
  liftIO $ Shroomz.diagnosticInfo initialApp
  var ← newTVarIO initialApp
  withRunInIO \runInIO → pure \request sendResponse → do
    app ← readTVarIO var ∷ IO (Shroomz m)
    let headersInfo = htmxInfo $ Wai.requestHeaders request
        pathInfo = Wai.pathInfo request
        path = maybe Path.Root Path.fromInfo $ NE.nonEmpty pathInfo
    case parseMethod (Wai.requestMethod request) of
      Right GET →
        sendResponse $ respondGet app headersInfo path
      Right POST → do
        body ← Wai.consumeRequestBodyLazy request
        (app', response) ← runInIO $ respondPost app path body
        atomically $ writeTVar var app'
        sendResponse response
      _ → sendResponse $ Wai.responseBuilder methodNotAllowed405 [] mempty

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
  updatedApp ←
    app & overComponentStateM path \Component {parseAction, update} state →
      case urlDecodeForm body of
        Left _err → pure state
        Right form → maybe (pure state) (update state) (parseAction form)
  let responseHeaders = []
      responseBody = Lucid.renderBS $ renderPath updatedApp path
  pure (updatedApp, Wai.responseLBS ok200 responseHeaders responseBody)

newtype HtmxInfo = HtmxInfo {isHtmxRequest ∷ Bool}

htmxInfo ∷ [Header] → HtmxInfo
htmxInfo headers = HtmxInfo {..}
 where
  isHtmxRequest =
    headers & any \(name, value) →
      name == "HX-Request" && value == "true"
