module Launcher where

import Data.Map.Strict qualified as Map
import Launcher.AppM (AppM)
import Launcher.Shroom.Layout (bodyWrapper)
import Launcher.Shroom.Processes qualified as Processes
import Lucid.Extended
import Shroomz (Shroomz, initApp)
import Shroomz.Component (Component (..), ComponentData (..), SomeComponent (..))
import Shroomz.Component.Path (ComponentPath, snocSlot)
import Shroomz.Component.Slot (Slot (..))
import Web.HttpApiData (toUrlPiece)
import Prelude hiding (State, state)

initApp ∷ AppM (Shroomz AppM)
initApp = Shroomz.initApp rootComponent bodyWrapper

type Action = ()
type State = ()

rootComponent ∷ Component AppM State Action
rootComponent =
  Component
    { parseAction = \_form → pass
    , update = \_state _action → pass
    , ..
    }
 where
  slot = SlotNamed "processes"

  initialise ∷ AppM (ComponentData AppM State)
  initialise =
    pure
      ComponentData
        { children = Map.singleton slot (SomeComponent Processes.component)
        , userState = ()
        }

  render ∷ ComponentPath → State → (Slot → Html_) → Html_
  render path () renderChild =
    div_ [class_ "component container"] do
      div_ [class_ "section"] do
        button_
          [ hxPost_ (toUrlPiece (snocSlot path slot))
          , hxTrigger_ "click"
          , hxTarget_ "next .processes"
          , hxAction_ Processes.Launch
          , class_ "button is-large"
          ]
          "Launch"
      div_ [class_ "section processes"] do
        renderChild slot
