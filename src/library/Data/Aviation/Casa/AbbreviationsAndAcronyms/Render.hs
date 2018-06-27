{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render where

import Control.Lens
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Prelude
import Data.List

data Colour =
  Colour
    (String -> String) -- heading separator
    (String -> String) -- heading name
    (String -> String) -- heading meaning
    (String -> String) -- heading source
    (String -> String) -- acronym separator
    (String -> String) -- acronym name
    (String -> String) -- acronym meaning
    (String -> String) -- acronym source

noColour ::
  Colour
noColour =
  Colour
    id
    id
    id
    id
    id
    id
    id
    id

class HasColour a where
  colour ::
    Lens'
      a
      Colour
  headingSeparatorColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSeparatorColour #-}
  headingSeparatorColour =
    colour . headingSeparatorColour
  headingNameColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingNameColour #-}
  headingNameColour =
    colour . headingNameColour
  headingMeaningColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingMeaningColour #-}
  headingMeaningColour =
    colour . headingMeaningColour
  headingSourceColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSourceColour #-}
  headingSourceColour =
    colour . headingSourceColour
  acronymSeparatorColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSeparatorColour #-}
  acronymSeparatorColour =
    colour . acronymSeparatorColour
  acronymNameColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymNameColour #-}
  acronymNameColour =
    colour . acronymNameColour
  acronymMeaningColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymMeaningColour #-}
  acronymMeaningColour =
    colour . acronymMeaningColour
  acronymSourceColour ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSourceColour #-}
  acronymSourceColour =
    colour . acronymSourceColour

instance HasColour Colour where
  colour =
    id
  headingSeparatorColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour x hn hm hs ac an am as) (f hc)
  headingNameColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc x hm hs ac an am as) (f hn)
  headingMeaningColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn x hs ac an am as) (f hm)
  headingSourceColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn hm x ac an am as) (f hs)
  acronymSeparatorColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn hm hs x an am as) (f ac)
  acronymNameColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn hm hs ac x am as) (f an)
  acronymMeaningColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn hm hs ac an x as) (f am)
  acronymSourceColour
    f (Colour hc hn hm hs ac an am as) =
      fmap (\x -> Colour hc hn hm hs ac an am x) (f as)

data Config =
  Config
    Colour

defaultConfig ::
  Config
defaultConfig =
  Config
    noColour

class HasConfig a where
  config ::
    Lens'
      a
      Config

instance HasConfig Config where
  config =
    id

instance HasColour Config where
  colour =
    lens
      (\(Config c) -> c)
      (\(Config _) c -> Config c)

newtype ConfigReader a =
  ConfigReader
    (Config -> a)

instance ConfigReader a_aaRr ~ t_aaRq =>
  Rewrapped (ConfigReader a_a86d) t_aaRq

instance Wrapped (ConfigReader a_a86d) where
  type Unwrapped (ConfigReader a_a86d) = Config -> a_a86d
  _Wrapped' = (iso (\(ConfigReader x_aaRp) -> x_aaRp)) ConfigReader

runConfig ::
  ConfigReader a
  -> Config
  -> a
runConfig (ConfigReader a) =
  a

instance Functor ConfigReader where
  fmap f (ConfigReader g) =
    ConfigReader (f . g)

instance Applicative ConfigReader where
  pure =
    ConfigReader . pure
  ConfigReader f <*> ConfigReader a =
    ConfigReader (\x -> f x (a x))

instance Monad ConfigReader where
  return =
    pure
  ConfigReader a >>= f =
    ConfigReader (\x -> runConfig (f (a x)) x)

readColour ::
  ConfigReader Colour
readColour =
  ConfigReader
    (^. colour)

readHeadingNameColour ::
  ConfigReader (String -> String)
readHeadingNameColour =
  (^. headingNameColour) <$> readColour

readHeadingMeaningColour ::
  ConfigReader (String -> String)
readHeadingMeaningColour =
  (^. headingMeaningColour) <$> readColour

readHeadingSourceColour ::
  ConfigReader (String -> String)
readHeadingSourceColour =
  (^. headingSourceColour) <$> readColour

readHeadingSeparatorColour ::
  ConfigReader (String -> String)
readHeadingSeparatorColour =
  (^. headingSeparatorColour) <$> readColour

readAcronymSeparatorColour ::
  ConfigReader (String -> String)
readAcronymSeparatorColour =
  (^. acronymSeparatorColour) <$> readColour

readAcronymNameColour ::
  ConfigReader (String -> String)
readAcronymNameColour =
  (^. acronymNameColour) <$> readColour

readAcronymMeaningColour ::
  ConfigReader (String -> String)
readAcronymMeaningColour =
  (^. acronymMeaningColour) <$> readColour

readAcronymSourceColour ::
  ConfigReader (String -> String)
readAcronymSourceColour =
  (^. acronymSourceColour) <$> readColour

renderHeader ::
  ConfigReader String
renderHeader =
  do  hc <- readHeadingSeparatorColour
      hn <- readHeadingNameColour
      hm <- readHeadingMeaningColour
      hs <- readHeadingSourceColour
      pure . intercalate (hc " ") $
        [
          hn (mkN 8 "ACRONYM")
        , hm (mkN 32 "MEANING")
        , hs (mkN 15 "SOURCE")
        ]

renderAcronym ::
  HasAcronym a =>
  a
  -> ConfigReader String
renderAcronym a =
  let name' =
        a ^. name
      meaning' =
        a ^. meaning
      source' =
        a ^. source
  in  do  sc <- readAcronymSeparatorColour
          an <- readAcronymNameColour
          am <- readAcronymMeaningColour
          as <- readAcronymSourceColour
          pure . intercalate (sc " ") $
            [
              an (mkN 8 name')
            , am (mkN 32 meaning')
            , as (mkN 15 source')
            ]

renderAcronyms ::
  HasAcronym a =>
  [a]
  -> ConfigReader String
renderAcronyms as =
  concat <$> traverse (\x -> (++ "\n") <$> renderAcronym x) as

renderHeaderAcronyms ::
  HasAcronym a =>
  [a]
  -> ConfigReader String
renderHeaderAcronyms as =
  do  h <- renderHeader
      a <- renderAcronyms as
      pure (h ++ "\n" ++ a)

mkN ::
  Int
  -> String
  -> String
mkN n x =
  let n' = n - length (take n x)
  in  x ++ replicate n' ' '
