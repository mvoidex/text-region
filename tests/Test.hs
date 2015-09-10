module Main (
	main
	) where

import Prelude.Unicode

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
			(≡ text) $ edit_ text $ do
				grouped $ do
					run $ cut bar
					run $ replace quux (by nums)
					run $ paste start (by xxx)
				undo >> redo >> undo >> undo >> undo
