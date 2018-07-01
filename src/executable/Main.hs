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
  let k :: [Fuzzy Acronym String]; k = searchFuzzyNameMeaningSource "BKN" "" "" False
      l = renderHeaderAcronyms k
      m = runConfig l (minimalSpacingStandardColours k)
  in  do  putStrLn m
          writeFile "/tmp/pp" m 
      