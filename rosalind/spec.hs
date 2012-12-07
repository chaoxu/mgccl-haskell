import System.IO
import Rosalind
import Data.Maybe
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ map (fromJust . proteinFromMass) $ reverse $ zipWith (-) (init xs) (tail xs)
               else do c <- getLine
                       loop ((read c::Double):xs)
