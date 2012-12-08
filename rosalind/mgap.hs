import Data.List
import Rosalind

main :: IO ()
main  = do s <- getLine
           t <- getLine 
           print $ length s + length t - (2 * length (lcs s t))
