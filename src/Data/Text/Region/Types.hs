{-# LANGUAGE TemplateHaskell, RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Data.Text.Region.Types (
	Point(..), pointLine, pointColumn, Size, (.-.), (.+.),
	Region(..), regionFrom, regionTo,
	Map(..),
	Contents, emptyContents,
	concatCts, splitCts, splitted,
	Editable(..), contents, by, measure,
	Replace(..), replaceRegion, replaceWith, Chain(..), chain, Edit,
	ActionIso(..), action, actionBack,
	ActionStack(..), undoStack, redoStack, emptyStack,
	EditState(..), editState, history, edited, regions,
	EditM(..),

	module Data.Group
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Category
import Control.Lens hiding ((.=))
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Group
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

-- | Point at text: zero-based line and column
data Point = Point {
	_pointLine ∷ Int,
	_pointColumn ∷ Int }
		deriving (Eq, Ord, Read, Show)

makeLenses ''Point

instance ToJSON Point where
	toJSON (Point l c) = object ["line" .= l, "column" .= c]

instance FromJSON Point where
	parseJSON = withObject "point" $ \v → Point <$> v .: "line" <*> v .: "column"

instance Monoid Point where
	mempty = Point 0 0
	Point l c `mappend` Point bl bc
		| l ≡ 0 = Point bl (c + bc)
		| otherwise = Point (l + bl) c

instance Group Point where
	invert (Point l c) = Point (negate l) (negate c)

-- | Distance between 'Point's is measured in lines and columns.
-- And it is defined, that distance between point at l:c and point (l + 1):0 is one line no matter c is
-- because we need to go to new line to reach destination point
-- Columns are taken into account only if points are on the same line
type Size = Point

-- | @pt .-. base@ is distance from @base@ to @pt@
-- Distance can't be less then zero lines and columns
(.-.) ∷ Point → Point → Point
Point l c .-. Point bl bc
	| bl < l = Point (l - bl) c
	| bl ≡ l = Point 0 (max 0 (c - bc))
	| otherwise = Point 0 0

-- | Opposite to ".-.", @(pt .-. base) .+. base = pt@
(.+.) ∷ Point → Point → Point
(Point l c) .+. (Point bl bc)
	| l ≡ 0 = Point bl (c + bc)
	| otherwise = Point (l + bl) c

-- | Region from 'Point' to another
data Region = Region {
	_regionFrom ∷ Point,
	_regionTo ∷ Point }
		deriving (Eq, Ord, Read, Show)

makeLenses ''Region

instance ToJSON Region where
	toJSON (Region f t) = object ["from" .= f, "to" .= t]

instance FromJSON Region where
	parseJSON = withObject "region" $ \v -> Region <$> v .: "from" <*> v .: "to"

-- | Main idea is that there are only two basic actions, that changes regions: inserting and cutting
-- When something is cutted out or inserted in, 'Region' positions must be updated
-- All editings can be represented as many cuts and inserts, so we can combine them to get function
-- which maps source regions to regions on updated data
-- Because insert is dual to cut (and therefore composes something like iso), we can also get function to map regions back
-- Combining this functions while edit, we get function, that maps regions from source data to edited one
-- To get back function, we must also combine opposite actions, or we can represent actions as 'Iso'
-- Same idea goes for modifying contents, represent each action as isomorphism and combine them together
newtype Map = Map { mapIso :: Iso' Region Region }

instance Monoid Map where
	mempty = Map $ iso id id
	Map l `mappend` Map r = Map (r . l)

instance Group Map where
	invert (Map f) = Map (from f)

-- | Contents is list of lines, list must have at least one (maybe empty) line
type Contents a = [a]

-- | Empty contents are contents with one empty line
emptyContents ∷ Monoid a ⇒ Contents a
emptyContents = [mempty]

checkCts ∷ Contents a → Contents a
checkCts [] = error "Contents can't be empty"
checkCts cs = cs

concatCts ∷ Monoid a ⇒ Contents a → Contents a → Contents a
concatCts ls rs = init (checkCts ls) ++ [last (checkCts ls) `mappend` head (checkCts rs)] ++ tail (checkCts rs)

-- | Split 'Contents' at some 'Point'
splitCts ∷ Editable a ⇒ Point → Contents a → (Contents a, Contents a)
splitCts (Point l c) cts = (take l cts ++ [p], s : drop (succ l) cts) where
	(p, s) = splitContents c (cts !! l)

-- | Get splitted 'Contents' at some 'Point'
splitted ∷ Editable a ⇒ Point → Iso' (Contents a) (Contents a, Contents a)
splitted p = iso (splitCts p) (uncurry concatCts)

-- | Something editable, string types implements this
class Monoid a ⇒ Editable a where
	-- | Split editable at some position
	splitContents ∷ Int → a → (a, a)
	contentsLength ∷ a → Int
	splitLines ∷ a → [a]
	joinLines ∷ [a] → a

-- | Get 'Contents' for some 'Editable', splitting lines
contents ∷ (Editable a, Editable b) ⇒ Iso a b (Contents a) (Contents b)
contents = iso splitLines joinLines

by ∷ Editable a ⇒ a → Contents a
by = splitLines

instance Editable String where
	splitContents = splitAt
	contentsLength = length
	splitLines s = case break (≡ '\n') s of
		(pre', "") → [pre']
		(pre', _:post') → pre' : splitLines post'
	joinLines = intercalate "\n"

instance Editable Text where
	splitContents = T.splitAt
	contentsLength = T.length
	splitLines = T.split (≡ '\n')
	joinLines = T.intercalate "\n"

-- | Contents 'Size'
measure ∷ Editable s ⇒ Contents s → Size
measure [] = error "Invalid input"
measure cts = Point (pred $ length cts) (contentsLength $ last cts)

-- | Serializable edit action
data Replace s = Replace {
	-- | 'Region' to replace
	_replaceRegion ∷ Region,
	-- | 'Contents' to replace with
	_replaceWith ∷ Contents s }
		deriving (Eq)

makeLenses ''Replace

instance (Editable s, ToJSON s) ⇒ ToJSON (Replace s) where
	toJSON (Replace e c) = object ["region" .= e, "contents" .= view (from contents) c]

instance (Editable s, FromJSON s) ⇒ FromJSON (Replace s) where
	parseJSON = withObject "edit" $ \v → Replace <$> v .: "region" <*> (view contents <$> v .: "contents")

instance (Editable s, ToJSON s) ⇒ Show (Replace s) where
	show = L.unpack ∘ encode

-- | Chain of edit actions
newtype Chain e s = Chain {
	_chain ∷ [e s] } deriving (Eq, Show, Monoid)

makeLenses ''Chain

instance ToJSON (e s) ⇒ ToJSON (Chain e s) where
	toJSON = toJSON ∘ _chain

instance FromJSON (e s) ⇒ FromJSON (Chain e s) where
	parseJSON = fmap Chain ∘ parseJSON

type Edit s = Chain Replace s

-- | Some action with its inverse
data ActionIso e = ActionIso {
	_action ∷ e,
	_actionBack ∷ e }

makeLenses ''ActionIso

instance Monoid e ⇒ Monoid (ActionIso e) where
	mempty = ActionIso mempty mempty
	ActionIso l l' `mappend` ActionIso r r' = ActionIso (l `mappend` r) (r' `mappend` l')

instance Monoid e ⇒ Group (ActionIso e) where
	invert (ActionIso f b) = ActionIso b f

instance ToJSON e ⇒ ToJSON (ActionIso e) where
	toJSON (ActionIso f b) = object ["fore" .= f, "back" .= b]

instance FromJSON e ⇒ FromJSON (ActionIso e) where
	parseJSON = withObject "action-iso" $ \v → ActionIso <$> v .: "fore" <*> v .: "back"

-- | Stack of undo/redo actions
data ActionStack e = ActionStack {
	_undoStack ∷ [ActionIso e],
	_redoStack ∷ [ActionIso e] }

makeLenses ''ActionStack

instance ToJSON e ⇒ ToJSON (ActionStack e) where
	toJSON (ActionStack u r) = object ["undo" .= u, "redo" .= r]

instance FromJSON e ⇒ FromJSON (ActionStack e) where
	parseJSON = withObject "action-stack" $ \v → ActionStack <$> v .: "undo" <*> v .: "redo"

emptyStack ∷ ActionStack e
emptyStack = ActionStack [] []

-- | Edit state
data EditState s r = EditState {
	-- | Edit history is stack of edit actions
	_history ∷ ActionStack (Edit s),
	-- | Currently edited data
	_edited ∷ Contents s,
	-- | Some region-based state, that will be updated on each edit
	_regions ∷ r }

makeLenses ''EditState

instance (Editable s, ToJSON s, ToJSON r) ⇒ ToJSON (EditState s r) where
	toJSON (EditState h e rs) = object ["history" .= h, "contents" .= view (from contents) e, "regions" .= rs ]

instance (Editable s, FromJSON s, FromJSON r) ⇒ FromJSON (EditState s r) where
	parseJSON = withObject "edit-state" $ \v → EditState <$> v .: "history" <*> fmap (view contents) (v .: "contents") <*> v .: "regions"

-- | Make edit state for contents
editState ∷ Editable s ⇒ s → r → EditState s r
editState x = EditState emptyStack (x ^. contents)

newtype EditM s r a = EditM { runEditM ∷ State (EditState s r) a } deriving (Applicative, Functor, Monad, MonadState (EditState s r))
