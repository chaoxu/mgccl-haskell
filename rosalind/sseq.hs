import Rosalind
import Data.Maybe
main :: IO ()
main  = do s <- getLine
           t <- getLine
           print $ show $ map (+1) $ fromJust $ seqList t s

seqList x y = if length x == length result then Just result else Nothing
  where seqList' (x:xs) (y:ys) n
          | x == y    = n: seqList' xs ys (n+1)
          | otherwise = seqList' (x:xs) ys (n+1)
        seqList' _ _ _ = []
        result = seqList' x y 0
