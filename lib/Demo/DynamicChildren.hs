module Demo.DynamicChildren (app) where

import Lucid.Extended
import Lucid.Hx
import Shroomz (Shroomz)
import Shroomz qualified
import Shroomz.Component (Component (..), StatelessComponent, statelessComponent)
import Shroomz.Component.Builder (setSlot)
import Shroomz.Component.Slot (Slot (..))

app ∷ Shroomz
app =
  Shroomz.new (statelessComponent demoRoot) do
    setSlot (SlotNamed "button") (statelessComponent button) Nothing
    setSlot (SlotNo 0) (statelessComponent child) Nothing
    setSlot (SlotNo 1) (statelessComponent child) Nothing

type Action = ()

demoRoot ∷ StatelessComponent Action
demoRoot =
  Component
    { render = \_path _state renderSlot → do
        div_ [class_ "container"] do
          h1_ [class_ "title"] "Dynamic children"
          div_ [class_ "child-named", class_ "box"] do
            h2_ [class_ "subtitle"] "Button component"
            renderSlot (SlotNamed "button")
          div_ [class_ "children-indexed", class_ "box"] do
            h2_ [class_ "subtitle"] "Indexed Children"
            div_ $ renderSlot (SlotNo 0)
            div_ $ renderSlot (SlotNo 1)
    , update = \_state _action → ()
    , parseAction = \_form → Nothing
    }

button ∷ StatelessComponent Action
button =
  Component
    { render = \_path _state _renderSlot → do
        div_ [hxTarget_ "this", hxSwap_ "outerHTML"] do
          form_ [] do
            div_ [class_ "buttons has-addons"] do
              button_
                [ hxPost_ "action"
                , class_ "button is-primary"
                ]
                "Add Child"
    , update = \_state _action → ()
    , parseAction = \_form → Nothing
    }

child ∷ StatelessComponent Action
child =
  Component
    { render = \_path _state _chilren → "Child Component"
    , update = \_state _action → ()
    , parseAction = \_form → Nothing
    }
