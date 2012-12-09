import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ editDistance c d
