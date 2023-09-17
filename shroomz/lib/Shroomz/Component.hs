module Shroomz.Component where

import Lucid.Extended (Html_, none_)
import Shroomz.Component.Path (ComponentPath)
import Shroomz.Component.Slot (Slot)
import Web.FormUrlEncoded (Form, lookupUnique)

data ComponentData m state = ComponentData
  { children ∷ Map Slot (SomeComponent m)
  , userState ∷ state
  }

data SomeComponent m
  = ∀ state action.
    SomeComponent
      (Component m state action)

withSomeComponent ∷ SomeComponent m → (∀ s a. Component m s a → r) → r
withSomeComponent (SomeComponent c) f = f c

data ComponentWithData m
  = ∀ state action.
    ComponentWithData
      (Component m state action)
      (ComponentData m state)

withComponentWithData
  ∷ ComponentWithData m
  → (∀ s a. Component m s a → ComponentData m s → r)
  → r
withComponentWithData (ComponentWithData c s) f = f c s

data Component m state action = Component
  { initialise ∷ m (ComponentData m state)
  , render ∷ ComponentPath → state → (Slot → Html_) → Html_
  , update ∷ state → action → m state
  , parseAction ∷ Form → Maybe action
  }

emptyComponent ∷ Applicative m ⇒ Component m () ()
emptyComponent =
  Component
    { initialise = pure ComponentData {children = mempty, userState = ()}
    , render = \_path _state _children → none_
    , update = \_state _action → pure _state
    , parseAction = \_form → pass
    }

parseActionField ∷ Read action ⇒ Text → Form → Maybe action
parseActionField field form = do
  value ← rightToMaybe $ lookupUnique field form
  readMaybe $ toString value
