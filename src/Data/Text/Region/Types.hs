{-# LANGUAGE TemplateHaskell, RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Data.Text.Region.Types (
	Point(..), pointLine, pointColumn, Size, (.-.), (.+.),
	Region(..), regionFrom, regionTo,
	Map(..),
	Contents,
	Edit(..), editUpdate, editMap,
	Prefix(..), prefixLines, prefixLine, Suffix(..), suffixLine, suffixLines, prefix, suffix, concatCts, splitCts,
	Editable(..), contents, measure,
	Replace(..), replaceRegion, replaceWith,
	ActionStack(..), undoStack, redoStack, emptyStack,
	EditState(..), editState, history, edited,
	EditorM(..),

	module Data.Group,
	module Control.Category.Inv
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Category
import Control.Lens hiding ((.=))
import Control.Monad.State
import Data.Aeson
import Data.Group
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Control.Category.Inv

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

type Size = Point

-- | Distance between points is measured in lines and columns.
-- And it is defined, that distance between point at l:c and point (l + 1):0 is one line no matter c is
-- because we need to go to new line to reach destination point
-- Columns are taken into account only if points are on the same line
-- @pt .-. base@ is distance from @base@ to @pt@
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

-- | Region from @Point@ to @Point@
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
-- When something is cutted out or inserted in, region positions must be updated
-- All editings can be represented as many cuts and inserts, so we can combine them to get function
-- which maps source regions to regions on updated data
-- Because insert is dual to cut (and therefore composes iso), we can also get function to map regions back
-- Combining this functions while edit, we get function, that maps regions from source data to edited one
-- To get back function, we must also combine opposite actions, or we can represent actions as isomorphisms
-- Same idea goes for modifying contents, represent each action as isomorphism and combine them together
newtype Map = Map { mapIso :: Iso' Region Region }

instance Monoid Map where
	mempty = Map $ iso id id
	Map l `mappend` Map r = Map (l . r)

instance Group Map where
	invert (Map f) = Map (from f)

-- | Contents is list of lines
type Contents a = [a]

data Edit a = Edit {
	_editUpdate ∷ Inv (Map, Contents a) (Map, Contents a),
	_editMap ∷ Map }

makeLenses ''Edit

instance Monoid (Edit a) where
	mempty = Edit id mempty
	Edit l lm `mappend` Edit r rm = Edit (l . r) (lm `mappend` rm)

data Prefix a = Prefix {
	_prefixLines ∷ [a],
	_prefixLine ∷ a }

makeLenses ''Prefix

instance Functor Prefix where
	fmap f (Prefix ls l) = Prefix (fmap f ls) (f l)

data Suffix a = Suffix {
	_suffixLine ∷ a,
	_suffixLines ∷ [a] }

makeLenses ''Suffix

instance Functor Suffix where
	fmap f (Suffix l ls) = Suffix (f l) (fmap f ls)

prefix ∷ Contents a → Prefix a
prefix cts = Prefix (init cts) (last cts)

suffix ∷ Contents a → Suffix a
suffix cts = Suffix (head cts) (tail cts)

concatCts ∷ Monoid a ⇒ Prefix a → Suffix a → Contents a
concatCts (Prefix ps p) (Suffix s ss) = ps ++ [p `mappend` s] ++ ss

splitCts ∷ Editable a ⇒ Point → Contents a → (Prefix a, Suffix a)
splitCts (Point l c) cts = (Prefix (take l cts) p, Suffix s (drop (succ l) cts)) where
	(p, s) = splitContents c (cts !! l)

class Monoid a ⇒ Editable a where
	splitContents ∷ Int → a → (a, a)
	contentsLength ∷ a → Int
	splitLines ∷ a → [a]
	joinLines ∷ [a] → a

contents ∷ (Editable a, Editable b) ⇒ Iso a b (Contents a) (Contents b)
contents = iso splitLines joinLines

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

-- | Contents size
measure ∷ Editable s ⇒ Contents s → Size
measure [] = error "Invalid input"
measure cts = Point (pred $ length cts) (contentsLength $ last cts)

-- | Serializable edit action
data Replace s = Replace {
	_replaceRegion ∷ Region,
	_replaceWith ∷ Contents s }
		deriving (Eq, Read, Show)

makeLenses ''Replace

instance (Editable s, ToJSON s) ⇒ ToJSON (Replace s) where
	toJSON (Replace e c) = object ["region" .= e, "contents" .= c]

instance (Editable s, FromJSON s) ⇒ FromJSON (Replace s) where
	parseJSON = withObject "edit" $ \v → Replace <$> v .: "region" <*> v .: "contents"

data ActionStack e = ActionStack {
	_undoStack ∷ [e],
	_redoStack ∷ [e] }

makeLenses ''ActionStack

emptyStack ∷ ActionStack e
emptyStack = ActionStack [] []

data EditState e s = EditState {
	_history ∷ ActionStack (e s),
	_edited ∷ Contents s }

makeLenses ''EditState

editState ∷ Editable s ⇒ s → EditState e s
editState x = EditState emptyStack (x ^. contents)

newtype EditorM e s a = EditorM { runEditorM ∷ State (EditState e s) a } deriving (Applicative, Functor, Monad, MonadState (EditState e s))
