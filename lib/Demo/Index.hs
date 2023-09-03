module Demo.Index (app) where

import Data.Map.Strict qualified as Map
import Demo.DynamicStatefulChildren qualified as DynamicStatefulChildren
import Demo.ParentChild qualified as ParentChild
import Demo.StatefulComponent qualified as StatefulComponent
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
import Shroomz (Shroomz, new)
import Shroomz.Component (Component (..), parseActionField, statelessComponent)
import Shroomz.Component.Path (ComponentPath, snocSlot)
import Shroomz.Component.Path qualified as Path
import Shroomz.Component.Slot (Slot (SlotNamed))
import Prelude hiding (State)

app ∷ Shroomz
app = Shroomz.new (statelessComponent indexComponent)

type State = ()

data Action = NavigateTo
  deriving stock (Show, Read)

indexComponent ∷ Component State Action
indexComponent =
  Component
    { render = _render
    , update = _update
    , parseAction = parseActionField "action"
    , children =
        Map.fromList
          [ (slotStatefulComponent, StatefulComponent.new)
          , (slotParentStatefulChild, ParentChild.new)
          , (slotDynamicStatefulChildren, DynamicStatefulChildren.new)
          ]
    }

_update ∷ State → Action → State
_update _ = \case
  NavigateTo → ()

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
        for_ tableData \(index, description, slot) → tr_ do
          let href = Path.render $ snocSlot path slot
          td_ $ fromString $ show index
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
