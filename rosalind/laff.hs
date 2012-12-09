import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ solve c d
    where solve c d = (localAffineAlignmentScore blosum62 11 1 c d, localAffineAlignmentBest blosum62 11 1 c d)
