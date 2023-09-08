module Demo.StatefulComponent
  ( new
  ) where

import Lucid.Extended
  ( Attributes
  , Html_
  , button_
  , class_
  , div_
  , ionIcon_
  , max_
  , name_
  , progress_
  , section_
  , value_
  )
import Lucid.Hx (hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Shroomz.Component
  ( Component (..)
  , ComponentWithState (..)
  , parseActionField
  , statefulComponent
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot)
import Web.HttpApiData (ToHttpApiData (toUrlPiece))
import Prelude hiding (State, state)

new ∷ ComponentWithState
new = statefulComponent counter 42

type State = Integer

data Action = Decrement | Increment
  deriving stock (Show, Read)

counter ∷ Component State Action
counter =
  Component
    { render = _render
    , update = _update
    , parseAction = parseActionField "action"
    , children = mempty
    }

_update ∷ State → Action → State
_update oldState = \case
  Decrement → 0 `max` oldState - step
  Increment → oldState + step `min` 100
 where
  step = 5

_render ∷ ComponentPath → State → (Slot → Html_) → Html_
_render path progress _renderSlot =
  div_ [class_ "container component"] do
    section_ [class_ "section"] do
      div_ [class_ "columns is-align-items-center"] do
        div_ [class_ "column is-one-third has-text-right"] do
          button_
            [ hxPost_ (toUrlPiece path)
            , hxTrigger_ "click"
            , hxTarget_ "closest .component"
            , hxSwap_ "outerHTML"
            , action_ Decrement
            , class_ "button is-large is-white"
            ]
            (ionIcon_ "remove-circle-outline" mempty)
        div_ [class_ "column"] do
          let progressClass
                | progress >= 66 = "is-success"
                | progress >= 33 = "is-warning"
                | otherwise = "is-danger"
          progress_
            [ class_ ("progress is-medium " <> progressClass)
            , value_ (show progress)
            , max_ "100"
            ]
            (show progress <> "%")
        div_ [class_ "column has-text-left"] do
          button_
            [ hxPost_ (toUrlPiece path)
            , hxTrigger_ "click"
            , hxTarget_ "closest .component"
            , hxSwap_ "outerHTML"
            , action_ Increment
            , class_ "button is-large is-white is-unselectable"
            ]
            (ionIcon_ "add-circle-outline" mempty)
 where
  action_ ∷ Action → Attributes
  action_ a = name_ "action" <> value_ (show a)
