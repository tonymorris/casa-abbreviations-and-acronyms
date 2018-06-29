{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render where

import Control.Lens hiding ((<|))
import Data.Align
import Data.These
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Prelude
import Data.Foldable
import Data.List
import Data.List.NonEmpty(NonEmpty, (<|))
import Data.Semigroup
import Data.Monoid.Textual(TextualMonoid)
import qualified Text.Fuzzy as Fuzzy(filter, score, original)
import Text.Fuzzy(Fuzzy(Fuzzy))

data Colours =
  Colours
    (String -> String) -- heading separator
    (String -> String) -- heading name
    (String -> String) -- heading meaning
    (String -> String) -- heading source
    (String -> String) -- heading score
    (String -> String) -- acronym separator
    (String -> String) -- acronym name
    (String -> String) -- acronym meaning
    (String -> String) -- acronym source
    (String -> String) -- acronym score

traverseAllColours ::
  Traversal'
    Colours
    (String -> String)
traverseAllColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

traverseSeparatorColours ::
  Traversal'
    Colours
    (String -> String)
traverseSeparatorColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseNameColours ::
  Traversal'
    Colours
    (String -> String)
traverseNameColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> f hn <*> pure hm <*> pure hs <*> pure hr <*> pure ac <*> f an <*> pure am <*> pure as <*> pure ar

traverseMeaningColours ::
  Traversal'
    Colours
    (String -> String)
traverseMeaningColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> f hm <*> pure hs <*> pure hr <*> pure ac <*> pure an <*> f am <*> pure as <*> pure ar

traverseSourceColours ::
  Traversal'
    Colours
    (String -> String)
traverseSourceColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> f hs <*> pure hr <*> pure ac <*> pure an <*> pure am <*> f as <*> pure ar

traverseScoreColours ::
  Traversal'
    Colours
    (String -> String)
traverseScoreColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> f ar

traverseHeadingColours ::
  Traversal'
    Colours
    (String -> String)
traverseHeadingColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseAcronymColours ::
  Traversal'
    Colours
    (String -> String)
traverseAcronymColours f (Colours hc hn hm hs hr ac an am as ar) =
  Colours <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

instance Semigroup Colours where
  Colours hc1 hn1 hm1 hs1 hr1 ac1 an1 am1 as1 ar1 <> Colours hc2 hn2 hm2 hs2 hr2 ac2 an2 am2 as2 ar2 =
    Colours (hc1 . hc2) (hn1 . hn2) (hm1 . hm2) (hs1 . hs2) (hr1 . hr2) (ac1 . ac2) (an1 . an2) (am1 . am2) (as1 . as2) (ar1 . ar2)

instance Monoid Colours where
  mappend =
    (<>)
  mempty =
    Colours
      id
      id
      id
      id
      id
      id
      id
      id
      id
      id

class HasColours a where
  colours ::
    Lens'
      a
      Colours
  headingSeparatorColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSeparatorColours #-}
  headingSeparatorColours =
    colours . headingSeparatorColours
  headingNameColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingNameColours #-}
  headingNameColours =
    colours . headingNameColours
  headingMeaningColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingMeaningColours #-}
  headingMeaningColours =
    colours . headingMeaningColours
  headingSourceColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSourceColours #-}
  headingSourceColours =
    colours . headingSourceColours
  headingScoreColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingScoreColours #-}
  headingScoreColours =
    colours . headingScoreColours
  acronymSeparatorColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSeparatorColours #-}
  acronymSeparatorColours =
    colours . acronymSeparatorColours
  acronymNameColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymNameColours #-}
  acronymNameColours =
    colours . acronymNameColours
  acronymMeaningColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymMeaningColours #-}
  acronymMeaningColours =
    colours . acronymMeaningColours
  acronymSourceColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSourceColours #-}
  acronymSourceColours =
    colours . acronymSourceColours
  acronymScoreColours ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymScoreColours #-}
  acronymScoreColours =
    colours . acronymScoreColours

instance HasColours Colours where
  colours =
    id
  headingSeparatorColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours x hn hm hs hr ac an am as ar) (f hc)
  headingNameColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc x hm hs hr ac an am as ar) (f hn)
  headingMeaningColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn x hs hr ac an am as ar) (f hm)
  headingSourceColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm x hr ac an am as ar) (f hs)
  headingScoreColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs x ac an am as ar) (f hr)
  acronymSeparatorColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr x an am as ar) (f ac)
  acronymNameColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac x am as ar) (f an)
  acronymMeaningColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an x as ar) (f am)
  acronymSourceColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an am x ar) (f as)
  acronymScoreColours
    f (Colours hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Colours hc hn hm hs hr ac an am as x) (f ar)

data Spacing =
  Spacing
    Int -- separator
    Int -- name
    Int -- meaning
    Int -- source
    Int -- score
  deriving (Eq, Ord, Show)

instance Semigroup Spacing where
  Spacing a1 b1 c1 d1 e1 <> Spacing a2 b2 c2 d2 e2 =
    Spacing (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)
    
instance Monoid Spacing where
  mappend =
    (<>)
  mempty =
    Spacing 0 0 0 0 0

class HasSpacing a where
  spacing ::
    Lens'
      a
      Spacing
  separatorSpacing ::
    Lens'
      a
      Int
  {-# INLINE separatorSpacing #-}
  separatorSpacing =
    spacing . separatorSpacing
  nameSpacing ::
    Lens'
      a
      Int
  {-# INLINE nameSpacing #-}
  nameSpacing =
    spacing . nameSpacing
  meaningSpacing ::
    Lens'
      a
      Int
  {-# INLINE meaningSpacing #-}
  meaningSpacing =
    spacing . meaningSpacing
  sourceSpacing ::
    Lens'
      a
      Int
  {-# INLINE sourceSpacing #-}
  sourceSpacing =
    spacing . sourceSpacing
  scoreSpacing ::
    Lens'
      a
      Int
  {-# INLINE scoreSpacing #-}
  scoreSpacing =
    spacing . scoreSpacing
  
instance HasSpacing Spacing where
  spacing =
    id
  separatorSpacing
    f (Spacing a b c d e) =
      fmap (\x -> Spacing x b c d e) (f a)
  nameSpacing
    f (Spacing a b c d e) =
      fmap (\x -> Spacing a x c d e) (f b)
  meaningSpacing
    f (Spacing a b c d e) =
      fmap (\x -> Spacing a b x d e) (f c)
  sourceSpacing
    f (Spacing a b c d e) =
      fmap (\x -> Spacing a b c x e) (f d)
  scoreSpacing
    f (Spacing a b c d e) =
      fmap (\x -> Spacing a b c d x) (f e)

----


standardColours ::
  Colours
standardColours =
  Colours
    (\s -> "\ESC[37m\ESC[101m" ++ s ++ "\ESC[m")
    (\s -> "\ESC[37m\ESC[101m" ++ s ++ "\ESC[m")
    id
    id
    id
    id
    id
    id
    id
    id
 
standardSpacing ::
  Spacing
standardSpacing =
  Spacing 1 15 75 25 5

standardConfig ::
  Config
standardConfig =
  Config
    standardColours
    standardSpacing

data Config =
  Config
    Colours
    Spacing

defaultConfig ::
  Config
defaultConfig =
  Config
    mempty
    mempty

class HasConfig a where
  config ::
    Lens'
      a
      Config

instance HasConfig Config where
  config =
    id

instance HasColours Config where
  colours f (Config c s) =
    fmap (\c' -> Config c' s) (f c)
    
instance HasSpacing Config where
  spacing f (Config c s) =
    fmap (\s' -> Config c s') (f s)

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

readColours ::
  ConfigReader Colours
readColours =
  ConfigReader
    (^. colours)

readHeadingSeparatorColours ::
  ConfigReader (String -> String)
readHeadingSeparatorColours =
  (^. headingSeparatorColours) <$> readColours

readHeadingNameColours ::
  ConfigReader (String -> String)
readHeadingNameColours =
  (^. headingNameColours) <$> readColours

readHeadingMeaningColours ::
  ConfigReader (String -> String)
readHeadingMeaningColours =
  (^. headingMeaningColours) <$> readColours

readHeadingSourceColours ::
  ConfigReader (String -> String)
readHeadingSourceColours =
  (^. headingSourceColours) <$> readColours

readHeadingScoreColours ::
  ConfigReader (String -> String)
readHeadingScoreColours =
  (^. headingScoreColours) <$> readColours

readAcronymSeparatorColours ::
  ConfigReader (String -> String)
readAcronymSeparatorColours =
  (^. acronymSeparatorColours) <$> readColours

readAcronymNameColours ::
  ConfigReader (String -> String)
readAcronymNameColours =
  (^. acronymNameColours) <$> readColours

readAcronymMeaningColours ::
  ConfigReader (String -> String)
readAcronymMeaningColours =
  (^. acronymMeaningColours) <$> readColours

readAcronymSourceColours ::
  ConfigReader (String -> String)
readAcronymSourceColours =
  (^. acronymSourceColours) <$> readColours

readAcronymScoreColours ::
  ConfigReader (String -> String)
readAcronymScoreColours =
  (^. acronymScoreColours) <$> readColours

readSpacing ::
  ConfigReader Spacing
readSpacing =
  ConfigReader
    (^. spacing) 

readSeparatorSpacing ::
  ConfigReader Int
readSeparatorSpacing =
  (^. separatorSpacing) <$> readSpacing

readNameSpacing ::
  ConfigReader Int
readNameSpacing =
  (^. nameSpacing) <$> readSpacing

readMeaningSpacing ::
  ConfigReader Int
readMeaningSpacing =
  (^. meaningSpacing) <$> readSpacing

readSourceSpacing ::
  ConfigReader Int
readSourceSpacing =
  (^. sourceSpacing) <$> readSpacing

readScoreSpacing ::
  ConfigReader Int
readScoreSpacing =
  (^. scoreSpacing) <$> readSpacing

renderHeader ::
  ConfigReader String
renderHeader =
  do  chc <- readHeadingSeparatorColours
      shc <- readSeparatorSpacing
      chn <- readHeadingNameColours
      shn <- readNameSpacing
      chm <- readHeadingMeaningColours
      shm <- readMeaningSpacing
      chs <- readHeadingSourceColours
      shs <- readSourceSpacing
      chr <- readHeadingScoreColours
      shr <- readScoreSpacing
      pure . intercalate (chc (replicate shc '|')) $
        [
          chn (spaceN shn "ACRONYM")
        , chm (spaceN shm  "MEANING")
        , chs (spaceN shs "SOURCE")
        , chr (spaceN shr "SCORE")
        ]

renderAcronym ::
  (HasScore a, HasAcronym a) =>
  a
  -> ConfigReader String
renderAcronym a =
  let name' =
        escapeChars (a ^. name)
      meaning' =
        escapeChars (a ^. meaning)
      source' =
        escapeChars (a ^. source)
      score' =
        show (a ^. score)
      spacesplit n x =
        toList $ spaceN n <$> splitEvery n x
  in  do  chc <- readHeadingSeparatorColours
          shc <- readSeparatorSpacing
          chn <- readHeadingNameColours
          shn <- readNameSpacing
          chm <- readHeadingMeaningColours
          shm <- readMeaningSpacing
          chs <- readHeadingSourceColours
          shs <- readSourceSpacing
          chr <- readHeadingScoreColours
          shr <- readScoreSpacing
          let name'' =
                spacesplit shn name'
              meaning'' =
                spacesplit shm meaning'
              source'' =
                spacesplit shs source'
              score'' =
                spacesplit shr score'
              alignWidth ::
                Align f =>
                (String -> String -> a)
                -> f String
                -> f String
                -> String
                -> String
                -> f a
              alignWidth k m n ms ns =
                alignWith
                  (\t -> case t of
                            This a1 ->
                              a1 `k` ns
                            That a2 ->
                              ms `k` a2
                            These a1 a2 ->
                              a1 `k` a2)
                  m
                  n
              spacers a1 a2 =
                a1 ++ chc (replicate shc '|') ++ a2
              column4 =
                let column12 =
                      alignWidth spacers (chn <$> name'') meaning'' (replicate shn ' ') (replicate shm ' ')
                    column3 =
                      alignWidth spacers column12 source'' (replicate (shn + shm) ' ') (replicate shs ' ')
                in  alignWidth spacers column3 score'' (replicate (shn + shm + shs) ' ') (replicate shr ' ')
          pure (newlines column4)

renderAcronyms ::
  (Traversable t, HasAcronym a, HasScore a) =>
  t a
  -> ConfigReader String
renderAcronyms as =
  concat <$> traverse renderAcronym as

renderHeaderAcronyms ::
  (Traversable t, HasAcronym a, HasScore a) =>
  t a
  -> ConfigReader String
renderHeaderAcronyms as=
  do  h <- renderHeader
      a <- renderAcronyms as
      pure (h ++ "\n" ++ a)

spaceN ::
  Int
  -> String
  -> String
spaceN n x =
  let n' = n - length x
  in  take n x ++ replicate n' ' '

escapeChars ::
  String
  -> String
escapeChars =
  transform
    (\x ->  case x of
              '&':'l':'t':';':r ->
                '<':r
              '&':'g':'t':';':r ->
                '>':r
              '&':'a':'m':'p':';':r ->
                '&':r
              '&':'q':'u':'o':'t':';':r ->
                '"':r
              _ ->
                x
              )

splitEvery ::
  Int
  -> String
  -> NonEmpty String
splitEvery w x =
  let (i, j) = splitAt w x
      k =
        case j of
          [] ->
            pure
          _:_ ->
            (<| splitEvery w j)
  in  k i

newlines ::
  [String]
  -> String
newlines s =
  s >>= (\t -> t ++ "\n")

nelHead ::
  Lens'
    (NonEmpty a)
    a
nelHead =
  _Wrapped . _1

nelTail ::
  Lens'
    (NonEmpty a)
    [a]
nelTail =
  _Wrapped . _2

nelTraverseTail ::
  Traversal'
    (NonEmpty a)
    a
nelTraverseTail =
  nelTail . traverse

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

alignWith3 ::
  Align f =>
  (These a (These b c) -> x)
  -> f a
  -> f b
  -> f c
  -> f x
alignWith3 f a b c =
  alignWith f a (align b c)

alignWith4 ::
  Align f =>
  (These a (These b (These c d)) -> x)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f x
alignWith4 f a b c d =
  alignWith3 f a b (align c d)
