import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ globalAffineAlignmentScore blosum62 0 5 c d
