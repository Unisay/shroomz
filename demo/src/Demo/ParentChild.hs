{-# LANGUAGE QuasiQuotes #-}

module Demo.ParentChild
  ( new
  ) where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Demo.StatefulComponent qualified as StatefulComponent
import Lucid.Extended
  ( Attributes
  , Html_
  , button_
  , class_
  , div_
  , id_
  , name_
  , none_
  , section_
  , style_
  , value_
  )
import Lucid.Hx (hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Shroomz.Component
  ( Component (..)
  , ComponentWithState
  , parseActionField
  , statefulComponent
  )
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot (..))
import Web.HttpApiData (ToHttpApiData (toUrlPiece))
import Prelude hiding (State, state)

new ∷ ComponentWithState
new = statefulComponent toggler Visible

data State = Visible | Hidden
  deriving stock (Show, Read)

matchState ∷ a → a → State → a
matchState visible hidden = \case
  Visible → visible
  Hidden → hidden

toggle ∷ State → State
toggle = matchState Hidden Visible

data Action = Toggle
  deriving stock (Show, Read)

toggler ∷ Component State Action
toggler =
  Component
    { render = _render
    , update = \oldState Toggle → toggle oldState
    , children = Map.singleton childSlot StatefulComponent.new
    , parseAction = parseActionField "action"
    }

_render ∷ ComponentPath → State → (Slot → Html_) → Html_
_render path state renderSlot =
  div_
    [ id_ "demo-parent"
    , class_ "container component"
    , hxSwap_ $ "outerHTML" <> matchState " swap:1s" "" state
    ]
    do
      section_ [class_ "section"] do
        button_
          [ hxPost_ (toUrlPiece path)
          , hxTrigger_ "click throttle:1s"
          , hxTarget_ "closest .component"
          , action_ Toggle
          , class_ "button is-normal"
          ]
          (matchState "Hide" "Show" state)
        style_ do
          [__i|
          .htmx-swapping .fade-me-in-out { opacity: 0; }
          .fade-me-in-out { opacity: 1; transition: opacity 1s ease-out; }
          |]
        section_
          [class_ "section fade-me-in-out"]
          case state of
            Hidden → none_
            Visible → div_ [class_ "box"] do renderSlot childSlot
 where
  action_ ∷ Action → Attributes
  action_ a = name_ "action" <> value_ (show a)

childSlot ∷ Slot
childSlot = SlotNamed "child"
