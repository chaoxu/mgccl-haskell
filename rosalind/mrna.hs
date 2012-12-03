import Data.List
import Data.List.Split
import Rosalind
import Data.Maybe
main :: IO ()
main  = do s <- getLine
           print $ foldl1' (\x y-> (x * y) `rem` 1000000) 
                 $ map (\x-> fromJust $ lookup x freq) ('X':s)


freq = [(x,length $ elemIndices x a)|x<- nub a]
    where a = map last $ chunksOf 4 rnaProteinTable 
