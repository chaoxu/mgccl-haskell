import Data.Maybe
import Newick
import Data.List
import Data.Set hiding (map) 
main :: IO ()
main = do c <- getLine
          putStr $ unlines $ solve $ newickTree c
  where solve t = map (char.fst) $ splits t
          where l    = sort $ leafLabels $ fmap fst t
                n    = length l
                h    = zip l [0..]
                char x = map (\y->if y `elem` z then '1' else '0') [0..n-1]
                  where z = map (\y->fromJust $ lookup y h) (toList x)
