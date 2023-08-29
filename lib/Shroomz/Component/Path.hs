module Shroomz.Component.Path
  ( ComponentPath (..)
  , parse
  , fromInfo
  , render
  , isAncestorOf
  , isDescendandOf
  , isParentOf
  , isChildOf
  , snocSlot
  , lastSlot
  , dropSlot
  , unsnocSlot
  ) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Shroomz.Component.Slot (Slot (..))
import Web.HttpApiData (ToHttpApiData (toUrlPiece))

{-

  / ->
    Root
  /parent ->
    Path (SlotNamed "parent") Nothing
  /parent/children/main ->
    Path (SlotNamed "parent") (Just (Path (SlotNamed "main") Nothing))
  /parent/children/0 ->
    Path (SlotNamed "parent") (Just (Path (SlotNo "0") Nothing))

 -}
data ComponentPath = Root | Path Slot (Maybe ComponentPath)
  deriving stock (Eq, Ord, Show)

instance ToHttpApiData ComponentPath where
  toUrlPiece = render

data InvalidPath = Doesn'tStartFromRoot
  deriving stock (Eq, Show)

parse ∷ Text → Either InvalidPath ComponentPath
parse path =
  case Text.uncons (Text.strip path) of
    Just ('/', rest) →
      Right
        if Text.null rest
          then Root
          else fromInfo $ NE.fromList (Text.splitOn "/" rest)
    _ → Left Doesn'tStartFromRoot

fromInfo ∷ NonEmpty Text → ComponentPath
fromInfo = \case
  "children" :| rest | Just slugs ← NE.nonEmpty rest → fromInfo slugs
  slug :| slugs → Path (parseSlot slug) (fromInfo <$> NE.nonEmpty slugs)
 where
  parseSlot ∷ Text → Slot
  parseSlot slug =
    case readMaybe (toString slug) of
      Just (n ∷ Natural) → SlotNo n
      Nothing → SlotNamed slug

render ∷ ComponentPath → Text
render = \case
  Root → "/"
  Path slot rest →
    ("/" <> renderSlot slot)
      & maybe identity ((<>) . render) rest
 where
  renderSlot = \case
    SlotNamed name → toUrlPiece name
    SlotNo n → toUrlPiece n

lastSlot ∷ ComponentPath → Maybe Slot
lastSlot Root = Nothing
lastSlot (Path slot Nothing) = Just slot
lastSlot (Path _slot (Just rest)) = lastSlot rest

snocSlot ∷ ComponentPath → Slot → ComponentPath
snocSlot Root slot = Path slot Nothing
snocSlot (Path pslot Nothing) slot = Path pslot (Just (Path slot Nothing))
snocSlot (Path pslot ppath) slot = Path pslot (flip snocSlot slot <$> ppath)

dropSlot ∷ ComponentPath → ComponentPath
dropSlot Root = Root
dropSlot (Path _slot Nothing) = Root
dropSlot (Path slot rest) = Path slot (dropSlot <$> rest)

unsnocSlot ∷ ComponentPath → Maybe (ComponentPath, Slot)
unsnocSlot Root = Nothing
unsnocSlot (Path slot Nothing) = Just (Root, slot)
unsnocSlot (Path slot (Just rest)) =
  case unsnocSlot rest of
    Nothing → Just (Root, slot)
    Just (rest', slot') → Just (Path slot (Just rest'), slot')

isParentOf ∷ ComponentPath → ComponentPath → Bool
isParentOf parent child = dropSlot child == parent

isChildOf ∷ ComponentPath → ComponentPath → Bool
isChildOf = flip isParentOf

isAncestorOf ∷ ComponentPath → ComponentPath → Bool
isAncestorOf Root Root = False
isAncestorOf Root _path = True
isAncestorOf (Path _slot _rest) Root = False
isAncestorOf (Path _pslot Nothing) (Path _cslot Nothing) = False
isAncestorOf (Path _pslot (Just _prest)) (Path _cslot Nothing) = False
isAncestorOf (Path pslot Nothing) (Path cslot (Just _)) = pslot == cslot
isAncestorOf (Path pslot (Just ppath)) (Path cslot (Just cpath)) =
  pslot == cslot && isAncestorOf ppath cpath

isDescendandOf ∷ ComponentPath → ComponentPath → Bool
isDescendandOf = flip isAncestorOf
