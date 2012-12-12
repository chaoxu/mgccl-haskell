import System.IO
import Rosalind
import Newick
import Data.Maybe
import Data.Tree
import Data.List

main :: IO ()
main  = do c <- getLine
           h' <- loop []
           let t = newickTree c
               words = map fst h'
               strings = map snd h'
               h = map (zip words) (transpose strings)
           putStr $ unlines $ concat $ zipWith (reversingSubstitutions t) h (map show [1..])

reversingSubstitutions :: Tree (String,Int) -> [(String,Char)] -> String -> [String]
reversingSubstitutions t h i = dfs "" 'X' "" 'Z' t
  where dfs p pc a ac (Node (w,_) sub)
         | pc == c = concatMap (dfs w c a ac) sub
         | ac == c = sol:concatMap (dfs w c w pc) sub
         | otherwise = concatMap (dfs w c w pc) sub
         where c = fromJust (lookup w h)
               sol = concat [a," ",w," ",i," ",[ac],"->",[pc],"->",[ac]]

loop xs = do end <- isEOF
             if end
               then return xs
               else do c <- readFASTA
                       loop (c:xs)
