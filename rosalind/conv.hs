import Data.List
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let a = read c :: [Double]
               b = read d :: [Double]
           print $ spectralConvolution a b
