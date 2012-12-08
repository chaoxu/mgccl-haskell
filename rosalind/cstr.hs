import System.IO
import Rosalind
import Data.List
import Data.List.Utils
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStr $ unlines $ solve $ transpose xs
               else do c <- getLine 
                       loop (c:xs)
  where solve xs  = filter nonTrivial $ map zeroone xs
        nonTrivial l = (count '0' l > 1) && (count '1' l > 1)
        zeroone xs
         | length (nub xs) == 1 = ""
         | otherwise            = replace [y] "1" $ replace [x] "0" xs 
         where [x,y] = nub xs
