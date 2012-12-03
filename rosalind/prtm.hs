import Data.List
import Data.Array
import Rosalind
main :: IO ()
main  = do s <- getLine
           print $ sum $ map (monoisotopicMassTable!) s
