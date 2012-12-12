-- TODO need to actually solve this problem in Haskell
import Rosalind
main :: IO ()
main  = do a <- getLine
           b <- getLine
           c <- getLine
           d <- getLine
           print $ sol [a,b,c,d]
    where sol xs = sum [-hammingDistance a b | a<-xs, b<-xs, a<b]
