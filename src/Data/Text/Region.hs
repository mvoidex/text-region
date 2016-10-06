{-# LANGUAGE RankNTypes, TupleSections, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Text.Region (
	pt, start, lineStart, regionLength, till, linesSize, regionLines, emptyRegion, line,
	regionSize, expandLines, atRegion, overlaps, applyMap, cutMap, insertMap,
	cutRegion, insertRegion,
	EditAction(..), cut, paste, overwrite, apply, update,

	module Data.Text.Region.Types
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Category
import Control.Lens

import Data.Text.Region.Types

-- | Make 'Point' from line and column
pt ∷ Int → Int → Point
pt = Point

-- | 'Point' at the beginning
start ∷ Point
start = pt 0 0

-- | 'Point' at the beginning of line
lineStart ∷ Int → Point
lineStart l = pt l 0

-- | Regions length
regionLength ∷ Lens' Region Size
regionLength = lens fromr tor where
	fromr (Region f t) = t .-. f
	tor (Region f _) sz = Region f (f .+. sz)

-- | Region from one 'Point' to another
till ∷ Point → Point → Region
l `till` r = Region (min l r) (max l r)

-- | Distance of @n@ lines
linesSize ∷ Int → Size
linesSize = pt 0

-- 'Region' height in lines, any 'Region' at least of line height 1
regionLines ∷ Lens' Region Int
regionLines = lens fromr tor where
	fromr (Region f t) = succ $ (t ^. pointLine) - (f ^. pointLine)
	tor (Region f t) l = Region f (set pointLine (f ^. pointLine + l) t)

-- | Is 'Region' empty
emptyRegion ∷ Region → Bool
emptyRegion r = r ^. regionFrom ≡ r ^. regionTo

-- | n'th line region, starts at the beginning of line and ends on the next line
line ∷ Int → Region
line l = lineStart l `till` lineStart (succ l)

-- | Make 'Region' by start position and 'Size'
regionSize ∷ Point → Size → Region
regionSize pt' sz = pt' `till` (pt' .+. sz)

-- | Expand 'Region' to contain full lines
expandLines ∷ Region → Region
expandLines (Region f t) = lineStart (f ^. pointLine) `till` lineStart (succ $ t ^. pointLine)

-- | Get contents at 'Region'
atRegion ∷ Editable s ⇒ Region → Lens' (Contents s) (Contents s)
atRegion r = lens fromc toc where
	fromc cts = cts ^. splitted (r ^. regionTo) . _1 . splitted (r ^. regionFrom) . _2
	toc cts cts' = (cts ^. splitted (r ^. regionFrom) . _1) `concatCts` cts' `concatCts` (cts ^. splitted (r ^. regionTo) . _2)

-- | Does regions overlaps
overlaps ∷ Region → Region → Bool
overlaps l r
	| r ^. regionFrom ≥ l ^. regionTo = False
	| r ^. regionTo ≤ l ^. regionFrom = False
	| otherwise = True

applyMap ∷ Map → Region → Region
applyMap = view ∘ mapIso

-- | Cut 'Region' mapping
cutMap ∷ Region → Map
cutMap rgn = Map $ iso (cutRegion rgn) (insertRegion rgn)

-- | Opposite to 'cutMap'
insertMap ∷ Region → Map
insertMap = invert ∘ cutMap

-- | Update second 'Region' position as if it was data cutted at first 'Region'
cutRegion ∷ Region → Region → Region
cutRegion (Region is ie) (Region s e) = Region
	(if is < s then (s .-. ie) .+. is else s)
	(if is < e then (e .-. ie) .+. is else e)

-- | Update second region position as if it was data inserted at first region (region sets insertion point and data size)
-- Region tries not to extend if data inserted at region bound except when region is empty
-- This allows define replace as cut and insert in special case when we replace region itself
insertRegion ∷ Region → Region → Region
insertRegion (Region is ie) (Region s e)
	| (s ≡ e) ∧ (is ≡ s) = Region is ie
	| otherwise = Region
		(if is ≤ s then (s .-. is) .+. ie else s)
		(if is < e then (e .-. is) .+. ie else e)

class Editable s ⇒ EditAction e s where
	-- | Make replace action over 'Region' and 'Contents'
	replace ∷ Region → Contents s → e s
	-- | Make 'Map' from action
	actionMap ∷ e s → Map
	-- | Perform action, modifying 'Contents'
	perform ∷ e s → Contents s → Contents s
	-- | Get action undo
	inversed ∷ e s → Contents s → e s

-- | Cuts region
cut ∷ EditAction e s ⇒ Region → e s
cut r = replace r emptyContents

-- | Pastes 'Contents' at some 'Point'
paste ∷ EditAction e s ⇒ Point → Contents s → e s
paste p = replace (p `till` p)

-- | Overwrites 'Contents' at some 'Point'
overwrite ∷ EditAction e s ⇒ Point → Contents s → e s
overwrite p c = replace (p `regionSize` measure c) c

-- | 'perform' for 'Edit'
apply ∷ Editable s ⇒ Edit s → Contents s → Contents s
apply = perform

-- | Update regions
update ∷ Editable s ⇒ Edit s → Region → Region
update = applyMap ∘ actionMap

instance Editable s ⇒ EditAction Replace s where
	replace = Replace
	actionMap (Replace r w) = insertMap (r & regionLength .~ measure w) `mappend` cutMap r
	perform (Replace r w) cts = cts & atRegion r .~ w
	inversed (Replace r w) cts = Replace (r & regionLength .~ measure w) (cts ^. atRegion r)

instance Editable s ⇒ EditAction Edit s where
	replace rgn txt = Edit [replace rgn txt]
	actionMap = foldr go mempty ∘ view replaces where
		go r m = actionMap (over replaceRegion (applyMap m) r) `mappend` m
	perform = snd ∘ foldr go (mempty, id) ∘ view replaces where
		go r (m, fn) = (actionMap r' `mappend` m, perform r' ∘ fn) where
			r' = over replaceRegion (applyMap m) r
	inversed e@(Edit rs) cts = Edit [Replace (applyMap m r) (cts ^. atRegion r) | Replace r _ ← rs] where
		m = actionMap e
