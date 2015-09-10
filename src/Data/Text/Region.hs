{-# LANGUAGE RankNTypes, TupleSections, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Text.Region (
	pt, start, lineStart, regionLength, till, linesSize, regionLines, emptyRegion, line,
	regionSize, expandLines, atRegion, ApplyMap(..), cutMap, insertMap,
	cutRegion, insertRegion,
	EditAction(..), cut, paste, overwrite, inverse, applyEdit, apply,
	edit, edit_, grouped, push, mapGrouped, run_, run, undo, redo, update,

	module Data.Text.Region.Types
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad.State
import Data.Maybe (isJust)

import Data.Text.Region.Types

pt ∷ Int → Int → Point
pt = Point

start ∷ Point
start = pt 0 0

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
	fromc cts = cts ^. splitted (r ^. regionTo) . _1 . splitted (r ^. regionFrom) . _2
	toc cts cts' = (cts ^. splitted (r ^. regionFrom) . _1) `concatCts` cts' `concatCts` (cts ^. splitted (r ^. regionTo) . _2)

class ApplyMap a where
	applyMap ∷ Map → a → a

instance ApplyMap Region where
	applyMap = view ∘ mapIso

instance ApplyMap Point where
	applyMap m p = view regionFrom $ applyMap m (p `till` p)

instance ApplyMap (Replace s) where
	applyMap m (Replace r w) = Replace (applyMap m r) w

instance ApplyMap (e s) ⇒ ApplyMap (Chain e s) where
	applyMap m (Chain rs) = Chain (map (applyMap m) rs)

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

class (Editable s, ApplyMap (e s)) ⇒ EditAction e s where
	replace ∷ Region → Contents s → e s
	actionMap ∷ e s → Map
	perform ∷ e s → State (Contents s) (e s)

cut ∷ EditAction e s ⇒ Region → e s
cut r = replace r emptyContents

paste ∷ EditAction e s ⇒ Point → Contents s → e s
paste p = replace (p `till` p)

overwrite ∷ EditAction e s ⇒ Point → Contents s → e s
overwrite p c = replace (p `regionSize` measure c) c

inverse ∷ EditAction e s ⇒ Contents s → e s → e s
inverse cts act = evalState (perform act) cts

applyEdit ∷ EditAction e s ⇒ e s → Contents s → Contents s
applyEdit act = snd ∘ runState (perform act)

apply ∷ EditAction Replace s ⇒ Edit s → Contents s → Contents s
apply = applyEdit

instance Editable s ⇒ EditAction Replace s where
	replace = Replace
	actionMap (Replace r w) = insertMap ((r ^. regionFrom) `regionSize` measure w) `mappend` cutMap r
	perform (Replace r w) = state $ \cts → (Replace ((r ^. regionFrom) `regionSize` measure w) (cts ^. atRegion r), atRegion r .~ w $ cts)

instance EditAction e s ⇒ EditAction (Chain e) s where
	replace rgn txt = Chain [replace rgn txt]
	actionMap (Chain []) = mempty
	actionMap (Chain (r : rs)) = actionMap (applyMap (actionMap r) (Chain rs)) `mappend` actionMap r
	perform (Chain rs) = (Chain ∘ reverse) <$> go mempty rs where
		go _ [] = return []
		go m (c : cs) = (:) <$> perform (applyMap m c) <*> go (actionMap (applyMap m c) `mappend` m) cs

edit ∷ EditAction Replace s ⇒ s → EditM s a → (a, s)
edit txt act = second (view $ edited . from contents) $ runState (runEditM act) (editState txt)

edit_ ∷ EditAction Replace s ⇒ s → EditM s a → s
edit_ txt = snd ∘ edit txt

grouped ∷ EditAction Replace s ⇒ EditM s a → EditM s a
grouped act = do
	inGroup ← gets (isJust . view groupMap)
	unless inGroup $ modify (set groupMap (Just mempty))
	x ← act
	unless inGroup $ modify (set groupMap Nothing)
	return x

push ∷ EditAction Replace s ⇒ Edit s → EditM s ()
push e = modify (over (history . undoStack) (e :)) >> modify (set (history . redoStack) [])

mapGrouped ∷ EditAction Replace s ⇒ Edit s → EditM s (Edit s)
mapGrouped e = do
	m ← gets (view groupMap)
	return $ maybe id applyMap m e

run_ ∷ EditAction Replace s ⇒ Edit s → EditM s (Edit s)
run_ e = do
	cts ← gets (view edited)
	let
		(undo', cts') = runState (perform e) cts
	modify (set edited cts')
	modify (over (groupMap . _Just) (mappend $ actionMap e))
	return undo'

run ∷ EditAction Replace s ⇒ Edit s → EditM s ()
run e = mapGrouped e >>= run_ >>= push

undo ∷ EditAction Replace s ⇒ EditM s ()
undo = do
	us@(~(u:_)) ← gets (view $ history . undoStack)
	unless (null us) $ do
		redo' ← run_ u
		modify (over (history . undoStack) tail)
		modify (over (history . redoStack) (redo' :))

redo ∷ EditAction Replace s ⇒ EditM s ()
redo = do
	rs@(~(r:_)) ← gets (view $ history . redoStack)
	unless (null rs) $ do
		undo' ← run_ r
		modify (over (history . redoStack) tail)
		modify (over (history . undoStack) (undo' :))

update ∷ (EditAction Replace s, ApplyMap a) ⇒ a → EditM s a
update x = do
	m ← gets (view groupMap)
	return $ maybe id applyMap m x
