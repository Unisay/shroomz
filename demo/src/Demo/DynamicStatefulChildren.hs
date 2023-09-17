module Demo.DynamicStatefulChildren
  ( component
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
  , ComponentData (..)
  , SomeComponent (..)
  , parseActionField
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot (SlotNo))
import Web.HttpApiData (toUrlPiece)
import Prelude hiding (State, state)

data State = State {titles ∷ [Text], currentTab ∷ Natural}

newtype Action = ActivateTab Natural
  deriving newtype (Show, Read)

component ∷ ∀ m. Applicative m ⇒ Component m State Action
component =
  Component
    { initialise = do
        let children ∷ Map Slot (SomeComponent m)
            children = Map.fromList do
              zip [0 ..] (fmap (toSnd makeTab) initialTitles)
                <&> \(i, (_title, c)) → (SlotNo i, SomeComponent c)
        pure
          ComponentData
            { userState = State {titles = initialTitles, currentTab = 0}
            , children = children
            }
    , render = _render
    , update = \state (ActivateTab i) → pure state {currentTab = i}
    , parseAction = parseActionField "tab"
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

data TabAction
type TabState = Natural

makeTab ∷ Applicative m ⇒ Text → Component m TabState TabAction
makeTab title =
  Component
    { initialise = pure ComponentData {children = mempty, userState = 0}
    , parseAction = \_form → Nothing
    , update = \(counter ∷ Natural) _action → pure (counter + 1)
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
