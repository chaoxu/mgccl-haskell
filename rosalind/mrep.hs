import Data.SuffixTree
import Data.List
main :: IO ()
main  = do c <- getLine
           putStr $ unlines $ solve (c++"$") `intersect` map reverse (solve $(reverse c)++"$")
  where solve c = filter (\x->length x >= 20) $ solutions "" $ constructWith "ACGT$" c
solutions s (Node a) = s: concatMap (\(x,y)-> solutions (s++prefix x) y) a
solutions _ (Leaf) = []

