import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ globalAffineAlignmentScore blosum62 5 0 c d
