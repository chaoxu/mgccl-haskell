import Data.Array
import Matrices
import LocalAlign

main :: IO ()
main  = do c <- getLine
           d <- getLine
           let (v,(a,b,_,_)) = localAffineAlignment blosum62 11 1 c d
           putStr $ unlines [show v,a,b]
