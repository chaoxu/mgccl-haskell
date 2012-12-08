import Rosalind
main :: IO ()
main  = do s <- getLine
           t <- getLine 
           putStrLn $ lcs s t
