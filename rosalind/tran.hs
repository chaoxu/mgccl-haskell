import Data.Maybe
import Rosalind
import Data.List
main :: IO ()
main  = do 
           c <- readFASTA
           d <- readFASTA
           let s = snd c
               t = snd d
           print $ transitionTransversionRatio s t
transitionTransversionRatio x y = fromIntegral (length (elemIndices 1 res))/
                                  fromIntegral (length (elemIndices (-1) res))
    where 
          res = zipWith mutation x y
          mutation a b
            | a == b     = 0
            | t == "AG" || t == "CT" = 1
            | otherwise = -1
            where t = sort [a,b]
