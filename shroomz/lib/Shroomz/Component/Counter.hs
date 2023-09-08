module Shroomz.Component.Counter where

import Web.HttpApiData

data Action = Increment | Decrement
  deriving stock (Eq, Show)

instance ToHttpApiData Action where
  toUrlPiece = \case
    Increment → "increment"
    Decrement → "decrement"

instance FromHttpApiData Action where
  parseUrlPiece = \case
    "increment" → Right Increment
    "decrement" → Right Decrement
    _ → Left "Invalid action"

data Query r = GetValue (Int → r) | SetValue Int r

{- new ∷ Component Query
new =
  mkComponent
    { initialState = (0 ∷ Int)
    , update = \st → \case
        Increment → pure $ succ st
        Decrement → pure $ pred st
    , render = \actionUrl _htmls st → do
        div_ [hxTarget_ "this", hxSwap_ "outerHTML", class_ "box"] do
          h1_ [class_ "title is-1"] "Counter"
          h2_ [class_ "title is-2"] $ "Value: " <> show st
          form_ [] do
            input_ [type_ "hidden", name_ "state", value_ (show st)]
            div_ [class_ "buttons has-addons"] do
              let button a =
                    button_ [hxPost_ (actionUrl a), class_ "button"] (show a)
              button Increment
              button Decrement
    , handleQuery = \case
        GetValue k → getState <&> k
        SetValue c k → modifyState (const c) $> k
    }
 -}
