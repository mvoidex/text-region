{-# LANGUAGE FlexibleContexts #-}

module Main (
	main
	) where

import Prelude.Unicode

import Control.Lens
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
			apply (cut quux `mappend` cut bar) text ≡ "foo baz"
		it "should perform undo" $
			let
				act' = mconcat [cut bar, replace quux nums, paste start xxx]
				undo' = undo act' text
			in
			(apply undo' ∘ apply act') text ≡ text
		it "should reverse text" $
			let
				go 0 _ txt = txt
				go n c txt = go (n - 1) (update act' c) (apply act' txt) where
					act' = mconcat [cut first, paste c (txt ^. contents . atRegion first . from contents)]
					first = pt 0 0 `till` pt 0 1
			in
			go (length text) (pt 0 (length text)) text ≡ reverse text
