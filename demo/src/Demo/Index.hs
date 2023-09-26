module Demo.Index (initApp) where

import Data.Map.Strict qualified as Map
import Demo.DynamicStatefulChildren qualified as DynamicStatefulChildren
import Demo.ParentChild qualified as ParentChild
import Demo.StatefulComponent qualified as StatefulComponent
import Demo.Wrapper qualified as Wrapper
import Lucid.Extended
  ( Html_
  , ToHtml (toHtml)
  , a_
  , class_
  , colspan_
  , div_
  , h1_
  , href_
  , ionIcon_
  , none_
  , style_
  , table_
  , target_
  , tbody_
  , td_
  , th_
  , thead_
  , tr_
  )
import Shroomz (BodyWrapper (..), Shroomz)
import Shroomz qualified
import Shroomz.Component
  ( Component (..)
  , ComponentData (..)
  , SomeComponent (..)
  , parseActionField
  )
import Shroomz.Component.Path (ComponentPath, snocSlot)
import Shroomz.Component.Path qualified as Path
import Shroomz.Component.Slot (Slot (SlotNamed))
import Prelude hiding (State)

initApp ∷ Monad m ⇒ m (Shroomz m)
initApp = Shroomz.initApp index BodyWrapper {wrapBody = Wrapper.wrapBody}

type State = ()

data Action = NavigateTo
  deriving stock (Show, Read)

index ∷ Applicative m ⇒ Component m State Action
index =
  Component
    { render = _render
    , update = _update
    , parseAction = parseActionField "action"
    , initialise =
        pure
          ComponentData
            { children =
                Map.fromList
                  [
                    ( slotStatefulComponent
                    , SomeComponent StatefulComponent.component
                    )
                  ,
                    ( slotParentStatefulChild
                    , SomeComponent ParentChild.component
                    )
                  ,
                    ( slotDynamicStatefulChildren
                    , SomeComponent DynamicStatefulChildren.component
                    )
                  ]
            , userState = ()
            }
    }

_update ∷ Applicative m ⇒ State → Action → m State
_update _ = \case
  NavigateTo → pure ()

_render ∷ ComponentPath → State → (Slot → Html_) → Html_
_render path _state _renderSlot =
  div_ [class_ "container component"] do
    h1_ [class_ "title"] "Shroomz showcase"
    table_ [class_ "table"] do
      thead_ do
        tr_ do
          th_ "#"
          th_ "Description"
          th_ [colspan_ "2"] "Link"
          th_ none_
      tbody_ do
        for_ tableData \(idx, description, slot) → tr_ do
          let href = Path.render $ snocSlot path slot
          td_ $ fromString $ show idx
          td_ description
          td_ [class_ "has-text-right"] do a_ [href_ href] (toHtml href)
          td_ [style_ "vertical-align:middle"] do
            a_ [href_ href, target_ "_blank"] do
              ionIcon_ "open-outline" (class_ "ml-1")
 where
  tableData ∷ [(Int, Html_, Slot)] =
    [
      ( 1
      , "Stateful component"
      , slotStatefulComponent
      )
    ,
      ( 2
      , "Parent component with one stateful child"
      , slotParentStatefulChild
      )
    ,
      ( 3
      , "Parent component with many stateful children"
      , slotDynamicStatefulChildren
      )
    ]

slotStatefulComponent ∷ Slot
slotStatefulComponent = SlotNamed "stateful-component"

slotParentStatefulChild ∷ Slot
slotParentStatefulChild = SlotNamed "parent-stateful-child"

slotDynamicStatefulChildren ∷ Slot
slotDynamicStatefulChildren = SlotNamed "dynamic-stateful-children"
