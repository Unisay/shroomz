module Lucid.Extended
  ( module Lucid
  , Html_
  , none_
  , classes_
  , ariaLabel_
  , ariaExpanded_
  , ariaHidden_
  , dataTarget_
  , ionIcon_
  ) where

import Lucid
import Lucid.Base (makeAttributes, makeElement)

type Html_ = Html ()

none_ ∷ Html_
none_ = pass

classes_ ∷ [Text] → Attributes
classes_ = class_ . unwords

ariaLabel_ ∷ Text → Attributes
ariaLabel_ = makeAttributes "aria-label"

ariaExpanded_ ∷ Bool → Attributes
ariaExpanded_ = makeAttributes "aria-expanded" . show

ariaHidden_ ∷ Bool → Attributes
ariaHidden_ = makeAttributes "aria-hidden" . show

dataTarget_ ∷ Text → Attributes
dataTarget_ = makeAttributes "data-target"

ionIcon_ ∷ Text → Html_
ionIcon_ name = makeElement "ion-icon" [name_ name] none_
