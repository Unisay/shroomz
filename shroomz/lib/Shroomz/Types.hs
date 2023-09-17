module Shroomz.Types where

import Data.Map.Strict qualified as Map
import Lucid.Extended (Html_)
import Shroomz.Component
  ( Component
  , ComponentData (..)
  , ComponentWithData (..)
  , withComponentWithData
  )
import Shroomz.Component.Path (ComponentPath)

data Shroomz m = Shroomz
  { components ∷ Map ComponentPath (ComponentWithData m)
  , bodyWrapper ∷ BodyWrapper
  }

newtype BodyWrapper = BodyWrapper {wrapBody ∷ Html_ → Html_}

--------------------------------------------------------------------------------
-- Lensy functions -------------------------------------------------------------

-- Modifications without a result ----------------------------------------------

overComponents
  ∷ ( Map ComponentPath (ComponentWithData m)
      → Map ComponentPath (ComponentWithData m)
    )
  → (Shroomz m → Shroomz m)
overComponents f s@Shroomz {components} = s {components = f components}

overComponent
  ∷ ComponentPath
  → (ComponentWithData m → ComponentWithData m)
  → (Shroomz m → Shroomz m)
overComponent path f = overComponents \components → do
  case Map.lookup path components of
    Nothing → components
    Just cd → Map.insert path (f cd) components

overComponentState
  ∷ ∀ m
   . ComponentPath
  → (∀ s a. Component m s a → s → s)
  → (Shroomz m → Shroomz m)
overComponentState path f = overComponent path (`withComponentWithData` adjust)
 where
  adjust ∷ ∀ s a. Component m s a → ComponentData m s → ComponentWithData m
  adjust c (d ∷ ComponentData m s) =
    ComponentWithData c d {userState = f c (userState d)}

-- Monadic modifications without a result --------------------------------------

overComponentsM
  ∷ Monad m
  ⇒ ( Map ComponentPath (ComponentWithData m)
      → m (Map ComponentPath (ComponentWithData m))
    )
  → (Shroomz m → m (Shroomz m))
overComponentsM f shroomz = do
  components' ← f (components shroomz)
  pure shroomz {components = components'}

overComponentM
  ∷ Monad m
  ⇒ ComponentPath
  → (ComponentWithData m → m (ComponentWithData m))
  → Shroomz m
  → m (Shroomz m)
overComponentM path f = overComponentsM \components → do
  case Map.lookup path components of
    Nothing → pure components
    Just cd → do
      cd' ← f cd
      pure $ Map.insert path cd' components

overComponentStateM
  ∷ ∀ m
   . Monad m
  ⇒ ComponentPath
  → (∀ s a. Component m s a → s → m s)
  → Shroomz m
  → m (Shroomz m)
overComponentStateM path f =
  overComponentM path (`withComponentWithData` adjust)
 where
  adjust ∷ ∀ s a. Component m s a → ComponentData m s → m (ComponentWithData m)
  adjust c (d ∷ ComponentData m s) = do
    userState' ← f c (userState d)
    pure $ ComponentWithData c d {userState = userState'}

-- Modifications with result ---------------------------------------------------

modifyComponents
  ∷ ( Map ComponentPath (ComponentWithData m)
      → (a, Map ComponentPath (ComponentWithData m))
    )
  → Shroomz m
  → (a, Shroomz m)
modifyComponents f shroomz =
  let (a, components') = f (components shroomz)
   in (a, shroomz {components = components'})

modifyComponent
  ∷ ComponentPath
  → (ComponentWithData m → (a, ComponentWithData m))
  → a
  -- ^ Default result if the component doesn't exist @ path
  → Shroomz m
  → (a, Shroomz m)
modifyComponent path f defaultResult = modifyComponents (Map.alterF alter path)
 where
  alter = \case
    Nothing → (defaultResult, Nothing)
    Just cd →
      let (a, cd') = f cd
       in (a, Just cd')

modifyComponentState
  ∷ ∀ m r
   . Monad m
  ⇒ ComponentPath
  → (∀ s a. Component m s a → s → (r, s))
  → r
  -- ^ Default result if the component doesn't exist @ path
  → Shroomz m
  → (r, Shroomz m)
modifyComponentState path f =
  modifyComponent path (`withComponentWithData` adjust)
 where
  adjust ∷ Component m s a → ComponentData m s → (r, ComponentWithData m)
  adjust c d =
    let (r, userState') = f c (userState d)
     in (r, ComponentWithData c d {userState = userState'})
