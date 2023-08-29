module Shroomz.Component.Counters where

type CounterName = Text

newtype Action = AddCounter CounterName

{- new ∷ Component Queryless
new =
  Component
    { initialState =
        mkSome (ComponentState (0 ∷ Int) mempty)
    , update = \case
        AddCounter _name → do
          modifyStateRaw \thisSt@ComponentState {children} →
            thisSt {children = children |> mkSome Counter.new}
    , render = \_actionUrl childrenHtml → \case
        Some (ComponentState i _) → do
          div_ $ "Sum of all counters: " <> show st
          fold childrenHtml
    , handleQuery = \case {}
    }
 -}
