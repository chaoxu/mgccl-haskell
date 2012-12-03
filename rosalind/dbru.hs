import System.IO
import Rosalind
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStrLn $ printEdges $ deBruijn xs
               else do c <- getLine
                       loop (c:xs)

printEdges [] = ""
printEdges ((x,y):xs) = '(':x++", "++y++")\n"++printEdges xs
