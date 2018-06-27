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
  let k = searchFuzzyNameMeaningSource "" "" "" False
      kk :: [Acronym]; kk = (\(Fuzzy o r s) -> o) <$> k
      l :: ConfigReader String; l = renderHeaderAcronyms kk
      ll = runConfig l defaultConfig
  in  do  putStrLn ll
          writeFile "/tmp/pp" ll
