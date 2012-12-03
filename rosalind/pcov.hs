import System.IO
import Rosalind
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ uncurry deBruijnString (head edges) edges
               else do c <- getLine
                       loop (c:xs)
    where edges = deBruijn xs
