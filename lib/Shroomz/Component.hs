module Shroomz.Component where

import Lucid.Extended (Html_, none_)
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot)

data ComponentWithState
  = ∀ state action. ComponentWithState (Component state action) state

statefulComponent ∷ ∀ s a. Component s a → s → ComponentWithState
statefulComponent = ComponentWithState

withComponentWithState ∷ ComponentWithState → (∀ s m. Component s m → s → r) → r
withComponentWithState (ComponentWithState c s) f = f c s

type StatelessComponent = Component ()

statelessComponent ∷ StatelessComponent action → ComponentWithState
statelessComponent c = ComponentWithState c ()

data Component state action = Component
  { render ∷ ComponentPath → state → (Slot → Html_) → Html_
  , update ∷ state → action → state
  , parseAction ∷ Text → Maybe action
  , children ∷ Map Slot ComponentWithState
  }

emptyComponent ∷ Component state action
emptyComponent =
  Component
    { render = \_path _state _children → none_
    , update = const
    , parseAction = \_input → Nothing
    , children = mempty
    }
