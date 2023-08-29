module Shroomz
  ( Shroomz
  , new
  , renderPath
  , updatePath
  ) where

import Data.Map.Strict qualified as Map
import Lucid.Extended (Html_)
import Shroomz.Component
  ( Component (..)
  , ComponentWithState (..)
  , withComponentWithState
  )
import Shroomz.Component.Builder (ComponentBuilder)
import Shroomz.Component.Builder qualified as Builder
import Shroomz.Component.Path (ComponentPath, isParentOf, lastSlot)
import Shroomz.Component.Path qualified as Path
import Shroomz.Component.Slot (Slot (..))
import Text.Show (Show (..))
import Web.FormUrlEncoded (lookupUnique, urlDecodeForm)
import Prelude hiding (get, show, state)

newtype Shroomz = Shroomz {components ∷ Map ComponentPath ComponentWithState}

-- | Create a new app with a single root component.
new ∷ ComponentWithState → ComponentBuilder → Shroomz
new root builder = Shroomz {components = Builder.build root builder}

findComponent ∷ Shroomz → ComponentPath → Maybe ComponentWithState
findComponent app path = Map.lookup path (components app)

getComponent ∷ Shroomz → ComponentPath → ComponentWithState
getComponent app path = fromMaybe noComponent $ findComponent app path
 where
  noComponent =
    ComponentWithState
      Component
        { render = \_path _state _children → "Component not found"
        , update = const
        , parseAction = const Nothing
        }
      ()

updateComponentState
  ∷ Shroomz → ComponentPath → (∀ s m. Component s m → s → s) → Shroomz
updateComponentState app path f =
  app {components = Map.adjust updateComponentWithState path (components app)}
 where
  updateComponentWithState cws = withComponentWithState cws \comp state →
    ComponentWithState comp (f comp state)

children ∷ Shroomz → ComponentPath → Map Slot ComponentWithState
children app parentPath =
  Map.fromList
    [ (childSlot, component)
    | (childPath, component) ← Map.toList (components app)
    , isParentOf parentPath childPath
    , childSlot ← maybeToList (lastSlot childPath)
    ]

renderPath ∷ Shroomz → ComponentPath → Html_
renderPath app path =
  withComponentWithState (getComponent app path) \comp state → do
    render comp path state \slot →
      case Map.lookup slot slotComponents of
        Nothing →
          "Component not found: "
            <> fromString (show path <> ":" <> show slot)
        Just child →
          renderChild (Path.snocSlot path slot) child
 where
  slotComponents = children app path

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
