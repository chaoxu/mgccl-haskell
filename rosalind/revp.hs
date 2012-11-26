import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           print $ revpal c

complement 'A' = 'T'
complement 'C' = 'G'
complement 'G' = 'C'
complement 'T' = 'A'

revpal xs = concat $ zipWith prod pos [map length (match!!x)|x<-pos]
  where match = map pali (tails xs)
        pos = findIndices (not . null) match
        prod x = map (reverse . (: [x + 1]))
pali xs = filter (\x-> x==rcomp x) [take i xs| i<-[4..min 12 $ length xs]]
rcomp xs = map complement $ reverse xs
