import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ globalAffineAlignmentScore blosum62 11 1 c d
