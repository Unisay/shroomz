module Shroomz.Component where

import Lucid.Extended (Html_)
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot)

data ComponentWithState
  = ∀ state action. ComponentWithState (Component state action) state

withComponentWithState ∷ ComponentWithState → (∀ s m. Component s m → s → r) → r
withComponentWithState (ComponentWithState c s) f = f c s

type StatelessComponent = Component ()

statelessComponent ∷ StatelessComponent action → ComponentWithState
statelessComponent c = ComponentWithState c ()

data Component state action = Component
  { render ∷ ComponentPath → state → (Slot → Html_) → Html_
  , update ∷ state → action → state
  , parseAction ∷ Text → Maybe action
  }
