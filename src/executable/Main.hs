module Main(
  main
) where

import Control.Applicative
import Control.Lens
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Colours
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Config
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Search
import Data.Semigroup((<>))
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))
import Options.Applicative(Parser, execParser, info, helper, fullDesc, header, option, maybeReader, short, long, value, metavar, help, switch, strOption)

main ::
  IO ()
main =
  {-
  let k :: [Fuzzy Acronym String]; k = searchFuzzyNameMeaningSource "BKN" "" "" False
      l = renderHeaderAcronyms k
      m = runConfig l (exactWidthSpacingStandardColours k)
  in  do  putStrLn m
          writeFile "/tmp/pp" m 
-}

  let opts =
        execParser
          (info (parserOptions <**> helper) (
            fullDesc <>
            header "casa-abbreviations-and-acronyms for searching CASA abbreviations and acronyms <https://www.casa.gov.au/about-us/standard-page/aviation-abbreviations-and-acronyms>"
          )
        )
  in  let k :: [Fuzzy Acronym String]; k = searchFuzzyNameMeaningSource "BKN" "" "" False
          l = renderHeaderAcronyms k
          m = runConfig l (exactWidthSpacingStandardColours k)
      in  do  putStrLn m
              putStrLn "===="
              o <- opts
              print o

data MatchField =
  MatchField
    Bool -- name
    Bool -- meaning
    Bool -- source
  deriving (Eq, Ord, Show)

parserMatchField ::
  Parser MatchField
parserMatchField =
  MatchField <$>
    switch
      (
        long "no-match-name" <>
        long "nn" <>
        help "do not match the acronym name"
      )
    <*>
    switch
      (
        long "no-match-meaning" <>
        long "nm" <>
        help "do not match the acronym meaning"
      )
    <*>
        switch
      (
        long "no-match-source" <>
        long "ns" <>
        help "do not match the acronym source"
      )

data MatchType =
  ExactMatch
  | InexactMatch (Maybe Int)
  deriving (Eq, Ord, Show)

parserMatchType ::
  Parser MatchType
parserMatchType =
  let opts exact minscore =
        if exact
          then
            ExactMatch
          else
            InexactMatch minscore
  in  opts <$>
        switch
          (
            short 'e' <>
            long "exact" <>
            help "match the search term exactly"
          ) <*>
        option
          (
            maybeReader
              (\s -> case reads s of
                        (n, _):_ ->
                          Just (Just n)
                        [] ->
                          Nothing)
          )
          (
            short 's' <>
            long "min-score" <>
            value Nothing <>
            help "minimum fuzzy match score"
          )

data FieldSpacing =
  FieldSpacing
    (Maybe Int) -- min name
    (Maybe Int) -- max name
    (Maybe Int) -- min meaning
    (Maybe Int) -- max meaning
    (Maybe Int) -- min source
    (Maybe Int) -- max source
    (Maybe Int) -- min score
    (Maybe Int) -- max score
  deriving (Eq, Ord, Show)    

parserFieldSpacing ::
  Parser FieldSpacing
parserFieldSpacing =
  let minmaxWidth longname1 longname2 helptext =
        option
          (
            maybeReader
              (\s -> case reads s of
                        (n, _):_ ->
                          Just (Just n)
                        [] ->
                          Nothing)

          )
          (
            long longname1 <>
            long longname2 <>
            value Nothing <>
            help helptext
          )
  in  FieldSpacing <$>
        minmaxWidth "min-name-width" "mn" "minimum acronym name column width"
        <*>
        minmaxWidth "max-name-width" "xn" "maximum acronym name column width"
        <*>
        minmaxWidth "min-meaning-width" "mm" "minimum acronym meaning column width"
        <*>
        minmaxWidth "max-meaning-width" "xm" "maximum acronym meaning column width"
        <*>
        minmaxWidth "min-source-width" "ms" "minimum acronym source column width"
        <*>
        minmaxWidth "max-source-width" "xs" "maximum acronym source column width"
        <*>
        minmaxWidth "min-score-width" "mr" "minimum score column width"
        <*>
        minmaxWidth "max-score-width" "xr" "maximum score column width"
        
data Options =
  Options
    Bool -- no colours
    Bool -- no header
    MatchField
    MatchType
    FieldSpacing
    [String] -- the search term
  deriving (Eq, Ord, Show)    

parserOptions ::
  Parser Options
parserOptions =
  Options <$>
    switch
      (
        short 'c' <>
        long "no-colour" <>
        help "turn off ANSI escape code colours"
      )
    <*>
    switch
      (
        short 'h' <>
        long "no-header" <>
        help "turn off the header in the output"
      )
    <*>
    parserMatchField
    <*>
    parserMatchType
    <*>
    parserFieldSpacing
    <*>
    some
      (
        strOption
          (
            short 't' <>
            long "term" <>
            help "the search term"
          )
      )

