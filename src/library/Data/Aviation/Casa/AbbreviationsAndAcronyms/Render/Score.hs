{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Score(
  HasScore(..)
) where

import Control.Category(id)
import Control.Lens(Lens')
import Data.Functor(fmap)
import Data.Int(Int)
import Data.Monoid.Textual(TextualMonoid)
import Text.Fuzzy(Fuzzy(Fuzzy))

class HasScore a where
  score ::
    Lens'
      a
      Int

instance HasScore Int where
  score =
    id

instance TextualMonoid s => HasScore (Fuzzy a s) where
  score f (Fuzzy x o s) =
    fmap (\t -> Fuzzy x o t) (f s)
