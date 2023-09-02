module Shroomz
  ( Shroomz
  , new
  , renderPath
  , updatePath
  , diagnosticInfo
  ) where

import Data.Map.Strict qualified as Map
import Lucid.Extended (Html_)
import Shroomz.Component
  ( Component (..)
  , ComponentWithState (..)
  , statelessComponent
  , withComponentWithState
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Path qualified as Path
import Web.FormUrlEncoded (lookupUnique, urlDecodeForm)
import Prelude hiding (get, state)

newtype Shroomz = Shroomz {components ∷ Map ComponentPath ComponentWithState}

-- | Create a new app with a single root component.
new ∷ ComponentWithState → Shroomz
new root = Shroomz {components = componentsFromRoot root}

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
        case lookupUnique "action" form of
          Left _err → state
          Right actionValue →
            case parseAction comp actionValue of
              Nothing → state
              Just action → update comp state action

diagnosticInfo ∷ MonadIO m ⇒ Shroomz → m ()
diagnosticInfo Shroomz {..} = liftIO do
  putTextLn "Shroomz components:"
  for_ (Map.toList components) \(path, _) → do
    putTextLn $ Path.render path
