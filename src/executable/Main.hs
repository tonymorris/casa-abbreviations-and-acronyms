module Main(
  main
) where

import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Search
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))

main ::
  IO ()
main =
  let k = searchFuzzyNameMeaningSource "BKN" "" "" False
      l = renderHeaderAcronyms ((\(Fuzzy o _ _) -> o) <$> k)
      c = defaultConfig
      m = runConfig l c
  in  do  putStrLn m
          writeFile "/tmp/pp" m
