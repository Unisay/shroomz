module Shroomz.Component.Builder where

import Data.Map.Strict qualified as Map
import Shroomz.Component (ComponentWithState)
import Shroomz.Component.Path (ComponentPath (..), snocSlot)
import Shroomz.Component.Slot (Slot (..))
import Text.Show (Show (..))
import Prelude hiding (show)

type ComponentBuilder = ComponentBuilderT Identity ()

data BuilderState = BuilderState
  { components ∷ !(Map ComponentPath ComponentWithState)
  , currentPath ∷ !ComponentPath
  }

instance Show BuilderState where
  show BuilderState {..} =
    "BuilderState { components = "
      <> show (Map.keys components)
      <> ", currentPath = "
      <> show currentPath
      <> "}"

newtype ComponentBuilderT m a
  = ComponentBuilderT (StateT BuilderState m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState BuilderState
    )

build ∷ ComponentWithState → ComponentBuilder → Map ComponentPath ComponentWithState
build root (ComponentBuilderT builder) =
  components $
    execState
      builder
      BuilderState
        { components = Map.singleton Root root
        , currentPath = Root
        }

setSlot ∷ Slot → ComponentWithState → Maybe ComponentBuilder → ComponentBuilder
setSlot slot component maybeNested = do
  modify' \st@BuilderState {..} →
    let nextPath = snocSlot currentPath slot
     in st {components = Map.insert nextPath component components}
  maybeNested & maybe pass \(ComponentBuilderT nested) → ComponentBuilderT do
    withState (\st → st {currentPath = snocSlot (currentPath st) slot}) nested
