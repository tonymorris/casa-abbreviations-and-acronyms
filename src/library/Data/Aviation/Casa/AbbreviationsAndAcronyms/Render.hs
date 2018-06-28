{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Render where

import Control.Lens hiding ((<|))
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Prelude
import Data.Foldable
import Data.List
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
import Data.Semigroup
import Data.Monoid.Textual(TextualMonoid)
import qualified Text.Fuzzy as Fuzzy(filter, score, original)
import Text.Fuzzy(Fuzzy(Fuzzy))

data Transforms =
  Transforms
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

mkTransforms ::
  (String -> String)
  -> Transforms
mkTransforms k =
  Transforms
    k
    k
    k
    k
    k
    k
    k
    k
    k
    k

traverseAllTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseAllTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

traverseSeparatorTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseSeparatorTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> f hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseNameTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseNameTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> pure hc <*> f hn <*> pure hm <*> pure hs <*> pure hr <*> pure ac <*> f an <*> pure am <*> pure as <*> pure ar

traverseMeaningTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseMeaningTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> pure hc <*> pure hn <*> f hm <*> pure hs <*> pure hr <*> pure ac <*> pure an <*> f am <*> pure as <*> pure ar

traverseSourceTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseSourceTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> pure hc <*> pure hn <*> pure hm <*> f hs <*> pure hr <*> pure ac <*> pure an <*> pure am <*> f as <*> pure ar

traverseScoreTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseScoreTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> f ar

traverseHeadingTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseHeadingTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> f hc <*> f hn <*> f hm <*> f hs <*> f hr <*> pure ac <*> pure an <*> pure am <*> pure as <*> pure ar

traverseAcronymTransforms ::
  Traversal'
    Transforms
    (String -> String)
traverseAcronymTransforms f (Transforms hc hn hm hs hr ac an am as ar) =
  Transforms <$> pure hc <*> pure hn <*> pure hm <*> pure hs <*> pure hr <*> f ac <*> f an <*> f am <*> f as <*> f ar

instance Semigroup Transforms where
  Transforms hc1 hn1 hm1 hs1 hr1 ac1 an1 am1 as1 ar1 <> Transforms hc2 hn2 hm2 hs2 hr2 ac2 an2 am2 as2 ar2 =
    Transforms (hc1 . hc2) (hn1 . hn2) (hm1 . hm2) (hs1 . hs2) (hr1 . hr2) (ac1 . ac2) (an1 . an2) (am1 . am2) (as1 . as2) (ar1 . ar2)

instance Monoid Transforms where
  mappend =
    (<>)
  mempty =
    Transforms
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
      
class HasTransforms a where
  transforms ::
    Lens'
      a
      Transforms
  headingSeparatorTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSeparatorTransforms #-}
  headingSeparatorTransforms =
    transforms . headingSeparatorTransforms
  headingNameTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingNameTransforms #-}
  headingNameTransforms =
    transforms . headingNameTransforms
  headingMeaningTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingMeaningTransforms #-}
  headingMeaningTransforms =
    transforms . headingMeaningTransforms
  headingSourceTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingSourceTransforms #-}
  headingSourceTransforms =
    transforms . headingSourceTransforms
  headingScoreTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE headingScoreTransforms #-}
  headingScoreTransforms =
    transforms . headingScoreTransforms
  acronymSeparatorTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSeparatorTransforms #-}
  acronymSeparatorTransforms =
    transforms . acronymSeparatorTransforms
  acronymNameTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymNameTransforms #-}
  acronymNameTransforms =
    transforms . acronymNameTransforms
  acronymMeaningTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymMeaningTransforms #-}
  acronymMeaningTransforms =
    transforms . acronymMeaningTransforms
  acronymSourceTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymSourceTransforms #-}
  acronymSourceTransforms =
    transforms . acronymSourceTransforms
  acronymScoreTransforms ::
    Lens'
      a
      (String -> String)
  {-# INLINE acronymScoreTransforms #-}
  acronymScoreTransforms =
    transforms . acronymScoreTransforms

instance HasTransforms Transforms where
  transforms =
    id
  headingSeparatorTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms x hn hm hs hr ac an am as ar) (f hc)
  headingNameTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc x hm hs hr ac an am as ar) (f hn)
  headingMeaningTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn x hs hr ac an am as ar) (f hm)
  headingSourceTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm x hr ac an am as ar) (f hs)
  headingScoreTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs x ac an am as ar) (f hr)
  acronymSeparatorTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs hr x an am as ar) (f ac)
  acronymNameTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs hr ac x am as ar) (f an)
  acronymMeaningTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs hr ac an x as ar) (f am)
  acronymSourceTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs hr ac an am x ar) (f as)
  acronymScoreTransforms
    f (Transforms hc hn hm hs hr ac an am as ar) =
      fmap (\x -> Transforms hc hn hm hs hr ac an am as x) (f ar)

spaceAllTransforms ::
  Int
  -> Transforms
spaceAllTransforms n =
  mempty & traverseAllTransforms .~ spaceN n

spaceSeparatorTransforms ::
  Int
  -> Transforms
spaceSeparatorTransforms n =
  mempty & traverseSeparatorTransforms .~ spaceN n
  
spaceNameTransforms ::
  Int
  -> Transforms
spaceNameTransforms n =
  mempty & traverseNameTransforms .~ spaceN n
  
spaceMeaningTransforms ::
  Int
  -> Transforms
spaceMeaningTransforms n =
  mempty & traverseMeaningTransforms .~ spaceN n
  
spaceSourceTransforms ::
  Int
  -> Transforms
spaceSourceTransforms n =
  mempty & traverseSourceTransforms .~ spaceN n
  
spaceScoreTransforms ::
  Int
  -> Transforms
spaceScoreTransforms n =
  mempty & traverseScoreTransforms .~ spaceN n
  
spaceHeadingTransforms ::
  Int
  -> Transforms
spaceHeadingTransforms n =
  mempty & traverseHeadingTransforms .~ spaceN n
  
spaceAcronymTransforms ::
  Int
  -> Transforms
spaceAcronymTransforms n =
  mempty & traverseAcronymTransforms .~ spaceN n
  
data Config =
  Config
    Transforms
    (Maybe Int)

defaultConfig ::
  Config
defaultConfig =
  Config
    mempty
    Nothing

class HasConfig a where
  config ::
    Lens'
      a
      Config
  maximumMeaningWidth ::
    Lens'
      a
      (Maybe Int)
  {-# INLINE maximumMeaningWidth #-}
  maximumMeaningWidth =
    config . maximumMeaningWidth

instance HasConfig Config where
  config =
    id
  maximumMeaningWidth
    f (Config t m) =
      fmap (\x -> Config t x) (f m)

instance HasTransforms Config where
  transforms =
    lens
      (\(Config c _) -> c)
      (\(Config _ m) c -> Config c m)

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

readTransforms ::
  ConfigReader Transforms
readTransforms =
  ConfigReader
    (^. transforms)

readHeadingSeparatorTransforms ::
  ConfigReader (String -> String)
readHeadingSeparatorTransforms =
  (^. headingSeparatorTransforms) <$> readTransforms

readHeadingNameTransforms ::
  ConfigReader (String -> String)
readHeadingNameTransforms =
  (^. headingNameTransforms) <$> readTransforms

readHeadingMeaningTransforms ::
  ConfigReader (String -> String)
readHeadingMeaningTransforms =
  (^. headingMeaningTransforms) <$> readTransforms

readHeadingSourceTransforms ::
  ConfigReader (String -> String)
readHeadingSourceTransforms =
  (^. headingSourceTransforms) <$> readTransforms

readHeadingScoreTransforms ::
  ConfigReader (String -> String)
readHeadingScoreTransforms =
  (^. headingScoreTransforms) <$> readTransforms

readAcronymSeparatorTransforms ::
  ConfigReader (String -> String)
readAcronymSeparatorTransforms =
  (^. acronymSeparatorTransforms) <$> readTransforms

readAcronymNameTransforms ::
  ConfigReader (String -> String)
readAcronymNameTransforms =
  (^. acronymNameTransforms) <$> readTransforms

readAcronymMeaningTransforms ::
  ConfigReader (String -> String)
readAcronymMeaningTransforms =
  (^. acronymMeaningTransforms) <$> readTransforms

readAcronymSourceTransforms ::
  ConfigReader (String -> String)
readAcronymSourceTransforms =
  (^. acronymSourceTransforms) <$> readTransforms

readAcronymScoreTransforms ::
  ConfigReader (String -> String)
readAcronymScoreTransforms =
  (^. acronymScoreTransforms) <$> readTransforms

readMaximumMeaningWidth ::
  ConfigReader (Maybe Int)
readMaximumMeaningWidth =
  ConfigReader
    (^. maximumMeaningWidth)

data Spacing =
  Spacing
    Int
    Int
    Int
    Int
    Int
  deriving (Eq, Ord, Show)

renderHeader ::
  Spacing
  -> ConfigReader String
renderHeader (Spacing shc shn shm shs shr) =
  do  hc <- readHeadingSeparatorTransforms
      hn <- readHeadingNameTransforms
      hm <- readHeadingMeaningTransforms
      hs <- readHeadingSourceTransforms
      hr <- readHeadingScoreTransforms
      mn <- readMaximumMeaningWidth
      let mnw =
            case mn of
              Nothing ->
                shm
              Just mx ->
                mx
      pure . intercalate (hc (replicate shc ' ')) $
        [
          hn (spaceN shn "ACRONYM")
        , hm (spaceN mnw "MEANING")
        , hs (spaceN shs "SOURCE")
        , hr (spaceN shr "SCORE")
        ]

renderAcronym ::
  HasAcronym a =>
  a
  -> Spacing
  -> ConfigReader String
renderAcronym a (Spacing shc shn shm shs shr) =
  let name' =
        escapeChars (a ^. name)
      meaning' =
        escapeChars (a ^. meaning)
      source' =
        escapeChars (a ^. source)
  in  do  sc <- readAcronymSeparatorTransforms
          an <- readAcronymNameTransforms
          am <- readAcronymMeaningTransforms
          as <- readAcronymSourceTransforms
          hr <- readHeadingScoreTransforms          
          mn <- readMaximumMeaningWidth
          let mnw =
                case mn of
                  Nothing ->
                    shm
                  Just mx ->
                    mx
          pure . concatMap (spaceN mnw) . toList $ splitEvery mnw meaning' & nelHead %~ (\mg ->
            intercalate (sc (replicate shc ' ')) $
              [
                an name'
              , am mg
              , as source'
              ])

renderAcronyms ::
  (Traversable t, HasAcronym a) =>
  t a
  -> Spacing
  -> ConfigReader String
renderAcronyms as sp =
  concat <$> traverse (\x -> (++ "\n") <$> renderAcronym x sp) as

renderHeaderAcronyms ::
  (Traversable t, HasAcronym a) =>
  t a
  -> Spacing
  -> ConfigReader String
renderHeaderAcronyms as sp =
  do  h <- renderHeader sp
      a <- renderAcronyms as sp
      pure (h ++ "\n" ++ a)

defaultSpaces ::
  Foldable t =>
  (a -> t b)
  -> [a]
  -> Int
defaultSpaces k a =
  case (length . k) <$> a of
    [] ->
      0
    r@(_:_) ->
      maximum r

spacesName ::
  TextualMonoid s =>
  [Fuzzy Acronym s]
  -> Transforms
spacesName =
  spaceNameTransforms . defaultSpaces (\x -> Fuzzy.original x ^. name)

spacesMeaning ::
  TextualMonoid s =>
  [Fuzzy Acronym s]
  -> Transforms
spacesMeaning =
  spaceMeaningTransforms . defaultSpaces (\x -> Fuzzy.original x ^. meaning)

spacesSource ::
  TextualMonoid s =>
  [Fuzzy Acronym s]
  -> Transforms
spacesSource =
  spaceSourceTransforms . defaultSpaces (\x -> Fuzzy.original x ^. source)

spacesScore ::
  TextualMonoid s =>
  [Fuzzy Acronym s]
  -> Transforms
spacesScore =
  spaceSourceTransforms . defaultSpaces (show . Fuzzy.score)

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

mid ::
  (a -> b -> b)
  -> Maybe a
  -> b
  -> b
mid =
  maybe id

ormax ::
  Ord a =>
  Maybe a
  -> a
  -> a
ormax =
  mid max