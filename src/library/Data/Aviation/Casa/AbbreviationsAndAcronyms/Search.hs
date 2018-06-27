{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Search where

import Control.Lens
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Data.Char
import Data.Foldable(foldl')
import Data.List(sortBy, filter)
import Data.Map(Map)
import qualified Data.Map as Map(fromList, lookup, insertWith, toList)
import Prelude
import Data.Monoid.Textual(TextualMonoid)
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))

all_Acronym_names_index ::
  Map String (String, String)
all_Acronym_names_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_name, (_meaning, _src))) <$> allAcronyms)

all_Acronym_meanings_index ::
  Map String (String, String)
all_Acronym_meanings_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_meaning, (_name, _src))) <$> allAcronyms)

all_Acronym_sources_index ::
  Map String (String, String)
all_Acronym_sources_index =
  Map.fromList ((\(Acronym _name _meaning _src) -> (_src, (_name, _meaning))) <$> allAcronyms)

searchIndexName ::
  String
  -> Maybe Acronym
searchIndexName s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_meaning, _src) -> Acronym s _meaning _src) <$> Map.lookup s' all_Acronym_names_index

searchIndexMeaning ::
  String
  -> Maybe Acronym
searchIndexMeaning s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _src) -> Acronym _name s _src) <$> Map.lookup s' all_Acronym_meanings_index

searchIndexSource ::
  String
  -> Maybe Acronym
searchIndexSource s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _meaning) -> Acronym s _name _meaning) <$> Map.lookup s' all_Acronym_sources_index

searchFuzzyName ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyName s before after cas =
  Fuzzy.filter s allAcronyms before after (^. name) cas

searchFuzzyMeaning ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyMeaning s before after cas =
  Fuzzy.filter s allAcronyms before after (^. meaning) cas

searchFuzzySource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzySource s before after cas =
  Fuzzy.filter s allAcronyms before after (^. source) cas

searchFuzzyNameMeaning ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameMeaning s before after cas =
  filterN s allAcronyms before after [(^. name), (^. meaning)] cas
  
searchFuzzyNameSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameSource s before after cas =
  filterN s allAcronyms before after [(^. name), (^. source)] cas

searchFuzzyMeaningSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyMeaningSource s before after cas =
  filterN s allAcronyms before after [(^. meaning), (^. source)] cas

searchFuzzyNameMeaningSource ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy Acronym String]
searchFuzzyNameMeaningSource s before after cas =
  filterN s allAcronyms before after [(^. name), (^. meaning), (^. source)] cas

-- https://hackage.haskell.org/package/fuzzy-0.1.0.0/docs/Text-Fuzzy.html#v:filter
filterN ::
  (Ord t, TextualMonoid s) =>
  s
  -> [t]
  -> s
  -> s
  -> [t -> s]
  -> Bool
  -> [Fuzzy t s]
filterN _ _ _ _ [] _ =
  []
filterN pattern values before after (e:es) cas =
  let x1 = Fuzzy.filter pattern values before after e cas
      x1' = Map.fromList ((\(Fuzzy o r s) -> (o, (r, s))) <$> x1)
      x2' = foldl' (\m e' ->  let x2 = Fuzzy.filter pattern values before after e' cas
                              in foldl' (\m' (Fuzzy o r s) -> Map.insertWith (\(s1, i1) (s2, i2) ->
                                    if i2 > i1 then (s2, i2) else (s1, i1)) o (r, s) m') m x2) x1' es
  in  sortBy (\f1 f2 -> Fuzzy.score f2 `compare` Fuzzy.score f1) ((\(o, (r, s)) -> Fuzzy o r s) <$> Map.toList x2')
