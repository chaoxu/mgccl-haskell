import System.IO
import Rosalind
import Data.List
import Data.Maybe
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ map fix err
               else do s <- getLine
                       loop (s:xs)
  where (e,c) = span ((==1).fst) $ sort $ zip (map length list) list
        err   = map (head.snd) e
        w = concatMap (nub.snd) c
        fix x = (x,fromJust $ find (\y-> hammingDistance x y == 1) (map reverseComplement w++w))
        list = groupBy equals $ sortBy compareDNA xs
          where equals x y = x==y || x == reverseComplement y
                compareDNA x y = compare (sort [x,reverseComplement x]) (sort [y,reverseComplement y])
