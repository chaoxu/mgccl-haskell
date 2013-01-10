import System.IO
import Data.Tree
import Data.List
import Control.Arrow hiding (loop)

main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStr $ unlines $ stringTries 1 (tries '_' xs)
               else do c <- getLine
                       loop (c:xs)

tries c xs = Node c (map (uncurry tries . f) g)
  where remain = sort $ map (head &&& tail) $ filter (not . null) xs
        g  = groupBy (\(x,_) (y,_) -> x==y) remain
        f ys = ((fst . head) ys, map snd ys)

stringTries i (Node _ xs) 
 | null xs   = []
 | otherwise = concat $ zipWith build vCounts xs
 where vCounts = scanl (+) (i+1) (map (length . flatten) xs)
       build x v@(Node a _) = (show i++" "++show x++" "++[a]) :stringTries x v
