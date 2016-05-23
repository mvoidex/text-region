{-# LANGUAGE RankNTypes, TupleSections, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Text.Region (
	pt, start, lineStart, regionLength, till, linesSize, regionLines, emptyRegion, line,
	regionSize, expandLines, atRegion, ApplyMap(..), updateMap, cutMap, insertMap,
	cutRegion, insertRegion,
	EditAction(..), cut, paste, overwrite, inverse, applyEdit, apply,
	edit, edit_, push, run_, run, runGroup, undo, redo,

	module Data.Text.Region.Types
	) where

import Prelude hiding (id, (.))
import Prelude.Unicode

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad.State

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

class ApplyMap a where
	applyMap ∷ Map → a → a

instance ApplyMap () where
	applyMap _ = id

instance ApplyMap a ⇒ ApplyMap [a] where
	applyMap m = map (applyMap m)

instance ApplyMap Map where
	applyMap = mappend

instance ApplyMap Region where
	applyMap = view ∘ mapIso

instance ApplyMap Point where
	applyMap m p = view regionFrom $ applyMap m (p `till` p)

instance ApplyMap (Replace s) where
	applyMap m (Replace r w) = Replace (applyMap m r) w

instance ApplyMap (e s) ⇒ ApplyMap (Chain e s) where
	applyMap m (Chain rs) = Chain (map (applyMap m) rs)

-- | Update 'Region' after some action
updateMap ∷ (EditAction e s, ApplyMap a) ⇒ e s → a → a
updateMap = applyMap ∘ actionMap

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

-- | Update second region position as if it was data inserted at first region
insertRegion ∷ Region → Region → Region
insertRegion (Region is ie) (Region s e) = Region
	(if is < s then (s .-. is) .+. ie else s)
	(if is < e then (e .-. is) .+. ie else e)

class (Editable s, ApplyMap (e s)) ⇒ EditAction e s where
	-- | Make replace action over 'Region' and 'Contents'
	replace ∷ Region → Contents s → e s
	-- | Make 'Map' from action
	actionMap ∷ e s → Map
	-- | Perform action, modifying 'Contents' and returning inverse (undo) action
	perform ∷ e s → State (Contents s) (e s)

-- | Cuts region
cut ∷ EditAction e s ⇒ Region → e s
cut r = replace r emptyContents

-- | Pastes 'Contents' at some 'Point'
paste ∷ EditAction e s ⇒ Point → Contents s → e s
paste p = replace (p `till` p)

-- | Overwrites 'Contents' at some 'Point'
overwrite ∷ EditAction e s ⇒ Point → Contents s → e s
overwrite p c = replace (p `regionSize` measure c) c

-- | Get undo-action
inverse ∷ EditAction e s ⇒ Contents s → e s → e s
inverse cts act = evalState (perform act) cts

-- | Apply action to 'Contents'
applyEdit ∷ EditAction e s ⇒ e s → Contents s → Contents s
applyEdit act = snd ∘ runState (perform act)

-- | 'applyEdit' for 'Edit'
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

-- | Run edit monad and return result with updated contents
edit ∷ EditAction Replace s ⇒ s → r → EditM s r a → (a, s)
edit txt rs act = second (view $ edited . from contents) $ runState (runEditM act) (editState txt rs)

-- | Run edit monad and return updated contents
edit_ ∷ EditAction Replace s ⇒ s → r → EditM s r a → s
edit_ txt rs = snd ∘ edit txt rs

-- | Push action into history, also drops redo stack
push ∷ ActionIso (Edit s) → EditM s r ()
push e = modify (over (history . undoStack) (e :)) >> modify (set (history . redoStack) [])

-- | Run edit action and returns corresponding redo-undo action
run_ ∷ (EditAction Replace s, ApplyMap r) ⇒ Edit s → EditM s r (ActionIso (Edit s))
run_ e = do
	cts ← gets (view edited)
	let
		(undo', cts') = runState (perform e) cts
	modify (set edited cts')
	modify (over regions (applyMap $ actionMap e))
	return $ ActionIso e undo'

-- | Run edit action with updating undo/redo stack
run ∷ (EditAction Replace s, ApplyMap r) ⇒ Edit s → EditM s r ()
run e = run_ e >>= push

-- | Run edit actions, updating undo/redo stack for each of them, but act like they was applied simultaneously
-- For example, cutting 1-st and then 3-rd letter:
-- @run (cut first) >> run (cut third) -- 1234 -> 234 -> 23@
-- @runGroup [cut first, cut third] -- 1234 -> 234 -> 24@
runGroup ∷ (EditAction Replace s, ApplyMap r) ⇒ [Edit s] → EditM s r ()
runGroup = go mempty where
	go _ [] = return ()
	go m (e:es) = run e' >> go (applyMap m $ actionMap e') es where
		e' = applyMap m e

-- | Undo last action
undo ∷ (EditAction Replace s, ApplyMap r) ⇒ EditM s r ()
undo = do
	us@(~(u:_)) ← gets (view $ history . undoStack)
	unless (null us) $ do
		_ ← run_ (u ^. actionBack)
		modify (over (history . undoStack) tail)
		modify (over (history . redoStack) (u :))

redo ∷ (EditAction Replace s, ApplyMap r) ⇒ EditM s r ()
redo = do
	rs@(~(r:_)) ← gets (view $ history . redoStack)
	unless (null rs) $ do
		_ ← run_ (r ^. action)
		modify (over (history . redoStack) tail)
		modify (over (history . undoStack) (r :))
