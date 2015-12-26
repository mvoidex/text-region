module Main (
	main
	) where

import Prelude.Unicode

import Control.Lens
import Control.Monad.State
import Data.Text.Region
import Test.Hspec

text ∷ String
text = "foo bar baz quux"

bar ∷ Region
bar = pt 0 3 `till` pt 0 7

quux ∷ Region
quux = pt 0 11 `till` pt 0 16

nums ∷ String
nums = " 123456"

xxx ∷ String
xxx = "xxx "

main ∷ IO ()
main = hspec $ do
	describe "regions are updated" $ do
		it "should delete correctly" $
			apply (cut quux `mappend` cut bar) (by text) ≡ by "foo baz"
	describe "editor monad" $ do
		it "should perform undo/redo" $
			(≡ text) $ edit_ text () $ do
				runGroup [
					cut bar,
					replace quux (by nums),
					paste start (by xxx)]
				undo >> redo >> undo >> undo >> undo
		it "should reverse text" $
			(≡ reverse text) $ edit_ text (pt 0 (length text)) $ replicateM_ (length text) $ do
				-- cut first letter and insert at caret
				let
					l = pt 0 0 `till` pt 0 1
				c ← gets (view regions)
				cts ← gets (view edited)
				run $ paste c (view (atRegion l) cts)
				run $ cut l
