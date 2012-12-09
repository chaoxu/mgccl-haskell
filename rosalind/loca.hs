import Data.Array
import Matrices
import Rosalind
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ solve c d
    where solve c d = (localAffineAlignmentScore pam250 0 5 c d, localAffineAlignmentBest pam250 0 5 c d)
