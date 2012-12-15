import Data.Array
import Matrices
import LocalAlign
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ localAffineAlignment pam250 0 5 c d
