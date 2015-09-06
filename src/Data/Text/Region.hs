{-# LANGUAGE RankNTypes #-}

module Data.Text.Region (
	pt, lineStart, regionLength, till, linesSize, regionLines, emptyRegion, line,
	regionSize, expandLines, atRegion, applyMap, applyStart, cutMap, insertMap,
	cutRegion, insertRegion,
	EditAction(..), undo, edit,
	replaceEdit,

	module Data.Text.Region.Types
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Arrow
import Control.Category
import Control.Lens

import Data.Text.Region.Types

pt ∷ Int → Int → Point
pt = Point

lineStart ∷ Int → Point
lineStart l = pt l 0

regionLength ∷ Lens' Region Size
regionLength = lens fromr tor where
	fromr (Region f t) = t .-. f
	tor (Region f _) sz = Region f (f .+. sz)

-- | Region from @Point@ to @Point@
till ∷ Point → Point → Region
l `till` r = Region (min l r) (max l r)

-- | Distance of @n@ lines
linesSize ∷ Int → Size
linesSize = pt 0

-- @Region@ height in lines, any @Region@ at least of line height 1
regionLines ∷ Lens' Region Int
regionLines = lens fromr tor where
	fromr (Region f t) = succ $ (t ^. pointLine) - (f ^. pointLine)
	tor (Region f t) l = Region f (set pointLine (f ^. pointLine + l) t)

-- | Is @Region@ empty
emptyRegion ∷ Region → Bool
emptyRegion r = r ^. regionFrom ≡ r ^. regionTo

-- | n'th line region, starts at the beginning of line and ends on the next line
line ∷ Int → Region
line l = lineStart l `till` lineStart (succ l)

-- | Make @Region@ by start position and @Size@
regionSize ∷ Point → Size → Region
regionSize pt' sz = pt' `till` (pt' .+. sz)

-- | Expand @Region@ to contain full lines
expandLines ∷ Region → Region
expandLines (Region f t) = lineStart (f ^. pointLine) `till` lineStart (succ $ t ^. pointLine)

-- | Get contents at @Region@
atRegion ∷ Editable s ⇒ Region → Lens' (Contents s) (Contents s)
atRegion r = lens fromc toc where
	fromc cts =
		over _head (snd ∘ splitContents (r ^. regionFrom . pointColumn)) ∘
		over _last (fst ∘ splitContents (r ^. regionTo . pointColumn)) ∘
		take (r ^. regionLines) ∘
		drop (r ^. regionFrom . pointLine) $
		cts
	toc cts cts' = snd $ (replace r cts' ^. editUpdate) `appInv` (mempty, cts)

-- | Apply mapping
applyMap ∷ Map → Region → Region
applyMap = view ∘ mapIso

-- | Apply mapping to starting position only
applyStart ∷ Map → Region → Region
applyStart m r = applyMap m $ (r ^. regionFrom) `regionSize` (r ^. regionLength)

-- | Cut @Region@ mapping
cutMap ∷ Region → Map
cutMap rgn = Map $ iso (cutRegion rgn) (insertRegion rgn)

-- | Opposite to @cut@
insertMap ∷ Region → Map
insertMap = invert ∘ cutMap

-- | Update second @Region@ position as if it was data cutted at first @Region@
cutRegion ∷ Region → Region → Region
cutRegion (Region is ie) (Region s e) = Region
	(if is < s then (s .-. ie) .+. is else s)
	(if is < e then (e .-. ie) .+. is else e)

-- | Update second region position as if it was data inserted at first region
insertRegion ∷ Region → Region → Region
insertRegion (Region is ie) (Region s e) = Region
	(if is < s then (s .-. is) .+. ie else s)
	(if is < e then (e .-. is) .+. ie else e)

class EditAction e where
	cut ∷ Editable s ⇒ Region → e s
	insert ∷ Editable s ⇒ Point → Contents s → e s
	replace ∷ Editable s ⇒ Region → Contents s → e s
	perform ∷ Editable s ⇒ Contents s → e s → (Contents s, e s)

undo ∷ (EditAction e, Editable s) ⇒ Contents s → e s → e s
undo cts act = snd $ cts `perform` act

edit ∷ (EditAction e, Editable s) ⇒ e s → Contents s → Contents s
edit act cts = fst $ cts `perform` act

instance EditAction Edit where
	cut r = Edit $ Inv cut' where
		cut' (m, cts) = ((m', cts'), view editUpdate $ insert (r ^. regionFrom) (cts ^. atRegion r')) where
			r' = m `applyMap` r
			m' = cutMap r' `mappend` m
			cts' = fst (splitCts (r' ^. regionFrom) cts) `concatCts` snd (splitCts (r' ^. regionTo) cts)
	insert p txt = Edit $ Inv insert' where
		insert' (m, cts) = ((m', cts'), view editUpdate $ cut (p `regionSize` measure txt)) where
			p' = (m `applyMap` (p `till` p)) ^. regionFrom
			m' = insertMap (p' `regionSize` measure txt) `mappend` m
			cts' = pre' `concatCts` (suffix $ prefix txt `concatCts` post') where
				(pre', post') = splitCts p' cts
	replace r txt = Edit $ Inv replace' where
		replace' (m, cts) = ((m', cts'), view editUpdate $ replace ((r ^. regionFrom) `regionSize` measure txt) (cts ^. atRegion r')) where
			r' = m `applyMap` r
			m' = insertMap ((r' ^. regionFrom) `regionSize` measure txt) `mappend` cutMap r' `mappend` m
			cts' = pre' `concatCts` (suffix $ prefix txt `concatCts` post') where
				pre' = fst (splitCts (r' ^. regionFrom) cts)
				post' = snd (splitCts (r' ^. regionTo) cts)
	perform cts (Edit f) = (view _2 *** Edit) $ runInv f (mempty, cts)

instance EditAction Replace where
	cut r = Replace r mempty
	insert p txt = Replace (p `till` p) txt
	replace = Replace
	perform cts (Replace r w) = (edit' (replace r w) cts, Replace ((r ^. regionFrom) `regionSize` measure w) (cts ^. atRegion r)) where
		edit' ∷ Editable s ⇒ Edit s → Contents s → Contents s
		edit' = edit

replaceEdit ∷ Editable s ⇒ Replace s → Edit s
replaceEdit r = replace (r ^. replaceRegion) (r ^. replaceWith)
