import System.IO
import Rosalind
import Data.List
import Data.Maybe
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ uncurry deBruijnString (head chosee) chosee
               else do c <- getLine
                       loop (c:xs)
    where list = map (works . edges) [n,n-1..1]
          chosek = n - fromJust (elemIndex True list)
          chosee = edges chosek
          n = length $ head xs
          edges k = deBruijn (kmers k xs)
          kmers k xs = nub $ filter (\x->length x == k) $ concatMap (map (take k) . tails) xs
          --all in degree is 1, 2 disjoint cycles...
          works e
            | null len || noc = False
            | otherwise       = last len == 1
            where occur = sort $ map fst e
                  len   = sort $ map length $ group occur 
                  noc  = not $ all (`elem` occur) $ nub $ map snd e

