module Main(
  main
) where

import Control.Lens
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Colours
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Config
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Render.Spacing
import Data.Aviation.Casa.AbbreviationsAndAcronyms.Search
import Data.Semigroup
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))

main ::
  IO ()
main =
  let k :: [Fuzzy Acronym String]; k = searchFuzzyNameMeaningSource "BKN" "" "" False
      l = renderHeaderAcronyms k
      m = runConfig l (exactWidthSpacingStandardColours k)
  in  do  putStrLn m
          writeFile "/tmp/pp" m 
      
-- match (any of) name, meaning, source
-- exact | maybe minimum score
-- no-colours
-- no-header
-- min and max spacing for each field
