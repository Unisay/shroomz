module Shroomz.Component where

import Lucid.Extended (Html_, none_)
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot)
import Web.FormUrlEncoded (Form, lookupUnique)

data ComponentWithState m
  = ∀ state action. ComponentWithState (Component m state action) state

statefulComponent ∷ ∀ m s a. Component m s a → s → ComponentWithState m
statefulComponent = ComponentWithState

withComponentWithState ∷ ComponentWithState m → (∀ s a. Component m s a → s → r) → r
withComponentWithState (ComponentWithState c s) f = f c s

type StatelessComponent m = Component m ()

statelessComponent ∷ StatelessComponent m action → ComponentWithState m
statelessComponent c = ComponentWithState c ()

data Component m state action = Component
  { render ∷ ComponentPath → state → (Slot → Html_) → Html_
  , update ∷ state → action → m state
  , parseAction ∷ Form → Maybe action
  , children ∷ Map Slot (ComponentWithState m)
  }

emptyComponent ∷ Applicative m ⇒ Component m state action
emptyComponent =
  Component
    { render = \_path _state _children → none_
    , update = \_state _action → pure _state
    , parseAction = \_form → Nothing
    , children = mempty
    }

parseActionField ∷ Read action ⇒ Text → Form → Maybe action
parseActionField field form = do
  value ← rightToMaybe $ lookupUnique field form
  readMaybe $ toString value
