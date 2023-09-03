module Demo.DynamicStatefulChildren
  ( new
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid.Extended
  ( Html_
  , ToHtml (toHtml)
  , a_
  , button_
  , class_
  , div_
  , form_
  , input_
  , li_
  , name_
  , type_
  , ul_
  , value_
  )
import Lucid.Hx (hxPost_, hxPushUrl_, hxSwap_, hxTarget_, hxTrigger_)
import Relude.Extra.Tuple (toSnd)
import Shroomz.Component
  ( Component (..)
  , ComponentWithState
  , parseActionField
  , statefulComponent
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot (SlotNo))
import Web.HttpApiData (toUrlPiece)
import Prelude hiding (State, state)

data State = State {titles ∷ [Text], currentTab ∷ Natural}

newtype Action = ActivateTab Natural
  deriving newtype (Show, Read)

new ∷ ComponentWithState
new = statefulComponent tabPanel State {titles = initialTitles, currentTab = 0}

tabPanel ∷ Component State Action
tabPanel =
  Component
    { render = _render
    , update = \state (ActivateTab i) → state {currentTab = i}
    , parseAction = parseActionField "tab"
    , children = Map.fromList do
        zip [0 ..] (fmap (toSnd makeTab) initialTitles) <&> \(i, (_title, c)) →
          (SlotNo i, c)
    }

_render ∷ ComponentPath → State → (Slot → Html_) → Html_
_render path State {..} renderSlot =
  div_ [class_ "container", hxSwap_ "outerHTML", hxTarget_ "this"] do
    div_ [class_ "tabs"] do
      ul_ do
        for_ (zip [0 ..] titles) \(index, title) → do
          li_ [class_ "is-active" | index == currentTab] do
            form_ [class_ "m-0"] do
              input_ [type_ "hidden", name_ "tab", value_ (show index)]
              let url = toUrlPiece path <> "?tab=" <> show index
              a_ [hxPost_ url, hxTrigger_ "click", hxPushUrl_ True] do
                toHtml title
    div_ [class_ "notification tab-contents"] do
      renderSlot (SlotNo currentTab)

initialTitles ∷ [Text]
initialTitles = ["Music", "Videos", "Documents", "Pictures"]

makeTab ∷ Text → ComponentWithState
makeTab title =
  statefulComponent
    Component
      { parseAction = const pass
      , children = mempty
      , update = \(counter ∷ Natural) (_action ∷ ()) → counter + 1
      , render = \path counter _renderSlot → do
          div_ [class_ "component content"] do
            div_ [class_ "block"] do
              toHtml do
                "Here you can see my "
                  <> Text.toLower title
                  <> ", all "
                  <> show counter
                  <> " items"
            div_ [class_ "block"] do
              button_
                [ class_ "button is-small"
                , hxSwap_ "outerHTML"
                , hxTrigger_ "click"
                , hxTarget_ "closest .component"
                , hxPost_ $ toUrlPiece path
                ]
                do "Click me to increment a counter"
      }
    0
