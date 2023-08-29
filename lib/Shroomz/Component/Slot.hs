module Shroomz.Component.Slot where

data Slot = SlotNamed Text | SlotNo Natural
  deriving stock (Eq, Ord, Show)
