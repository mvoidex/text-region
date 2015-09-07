{-# LANGUAGE RankNTypes, TupleSections #-}

module Data.Text.Region (
	pt, lineStart, regionLength, till, linesSize, regionLines, emptyRegion, line,
	regionSize, expandLines, atRegion, applyMap, applyStart, cutMap, insertMap,
	cutRegion, insertRegion,
	EditAction(..), undo, edit,
	replaceEdit, replaces,
	editor, editor_, performEdit, doEdit, undoEdit, redoEdit,

	module Data.Text.Region.Types
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad.State

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
	regionMap ∷ Editable s ⇒ e s → Region → Region

undo ∷ (EditAction e, Editable s) ⇒ Contents s → e s → e s
undo cts act = snd $ cts `perform` act

edit ∷ (EditAction e, Editable s) ⇒ e s → Contents s → Contents s
edit act cts = fst $ cts `perform` act

mkEdit ∷ Editable s ⇒ (Map → Contents s → (Contents s, Edit s)) → Map → Edit s
mkEdit fn m = Edit (Inv fn') m where
	fn' (m', cts') = ((m `mappend` m', ) *** view editUpdate) $ fn m' cts'

instance EditAction Edit where
	cut r = mkEdit cut' (cutMap r) where
		cut' m cts = (cts', insert (r ^. regionFrom) (cts ^. atRegion r')) where
			r' = m `applyMap` r
			cts' = fst (splitCts (r' ^. regionFrom) cts) `concatCts` snd (splitCts (r' ^. regionTo) cts)
	insert p txt = mkEdit insert' (insertMap (p `regionSize` measure txt)) where
		insert' m cts = (cts', cut (p `regionSize` measure txt)) where
			p' = (m `applyMap` (p `till` p)) ^. regionFrom
			cts' = pre' `concatCts` suffix (prefix txt `concatCts` post') where
				(pre', post') = splitCts p' cts
	replace r txt = mkEdit replace' (insertMap ((r ^. regionFrom) `regionSize` measure txt) `mappend` cutMap r) where
		replace' m cts = (cts', replace ((r ^. regionFrom) `regionSize` measure txt) (cts ^. atRegion r')) where
			r' = m `applyMap` r
			cts' = pre' `concatCts` suffix (prefix txt `concatCts` post') where
				pre' = fst (splitCts (r' ^. regionFrom) cts)
				post' = snd (splitCts (r' ^. regionTo) cts)
	perform cts (Edit f m) = (view _2 *** (`Edit` invert m)) $ runInv f (mempty, cts)
	regionMap (Edit _ m) = applyMap m

instance EditAction Replace where
	cut r = Replace r [mempty]
	insert p = Replace (p `till` p)
	replace = Replace
	perform cts (Replace r w) = (edit' (replace r w) cts, Replace ((r ^. regionFrom) `regionSize` measure w) (cts ^. atRegion r)) where
		edit' ∷ Editable s ⇒ Edit s → Contents s → Contents s
		edit' = edit
	regionMap (Replace r w) = applyMap (insertMap ((r ^. regionFrom) `regionSize` measure w) `mappend` cutMap r)

replaceEdit ∷ Editable s ⇒ Replace s → Edit s
replaceEdit r = replace (r ^. replaceRegion) (r ^. replaceWith)

replaces ∷ Editable s ⇒ [Replace s] → Edit s
replaces = mconcat ∘ map replaceEdit

editor ∷ (EditAction e, Editable s) ⇒ s → EditorM e s a → (a, s)
editor txt act = second (view $ edited . from contents) $ runState (runEditorM act) (editState txt)

editor_ ∷ (EditAction e, Editable s) ⇒ s → EditorM e s a → s
editor_ txt = snd ∘ editor txt

performEdit ∷ (EditAction e, Editable s) ⇒ e s → EditorM e s (e s)
performEdit edit' = do
	cts ← gets (view edited)	
	let
		(cts', undo') = perform cts edit'
	modify (set edited cts')
	return undo'

doEdit ∷ (EditAction e, Editable s) ⇒ e s → EditorM e s ()
doEdit edit' = do
	undo' ← performEdit edit'
	modify (over (history . undoStack) (undo' :))
	modify (set (history . redoStack) [])

undoEdit ∷ (EditAction e, Editable s) ⇒ EditorM e s ()
undoEdit = do
	us@(~(u:_)) ← gets (view $ history . undoStack)
	unless (null us) $ do
		redo' ← performEdit u
		modify (over (history . undoStack) tail)
		modify (over (history . redoStack) (redo' :))

redoEdit ∷ (EditAction e, Editable s) ⇒ EditorM e s ()
redoEdit = do
	rs@(~(r:_)) ← gets (view $ history . redoStack)
	unless (null rs) $ do
		undo' ← performEdit r
		modify (over (history . redoStack) tail)
		modify (over (history . undoStack) (undo' :))
