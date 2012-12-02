import Rosalind
main :: IO ()
main  = do s <- getLine
           t <- getLine 
           print (editDistance s t,editString s t)
