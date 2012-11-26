import System.IO
import Data.List
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
              then print (str, table)
              else do x <- getLine
                      loop (x:xs)
  where table = map (\y-> map (length . filter (==y)) (transpose xs)) "ACGT"
        str = zipWith (\x y->"ACGT"!!head (elemIndices x y)) (map maximum z) z
        z = transpose table
