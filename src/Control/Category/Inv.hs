{-# LANGUAGE RankNTypes #-}

module Control.Category.Inv (
	Inv(..), fromIso, inverse, appInv
	) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Lens

-- | (inverse x f) `appInv` (f `appInv` x) ≡ x
data Inv a b = Inv { runInv ∷ a → (b, Inv b a) }

instance Category Inv where
	id = Inv $ \x → (x, id)
	Inv f . Inv g = Inv $ \x → ((fst . f . fst . g) x, snd (g x) . snd (f (fst (g x))))
	
fromIso ∷ Iso' a b → Inv a b
fromIso iso' = Inv $ \x → (x ^. iso', inv') where
	inv' = Inv $ \x' → (x' ^. from iso', fromIso iso')

inverse ∷ a → Inv a b → Inv b a
inverse x inv = snd $ runInv inv x

appInv ∷ Inv a b → (a → b)
appInv inv = fst . runInv inv
