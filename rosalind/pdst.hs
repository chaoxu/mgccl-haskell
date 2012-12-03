import System.IO
import Rosalind

main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print [[pDistance x y|x<-reverse xs]|y<-reverse xs]
               else do (_,t) <- readFASTA
                       loop (t:xs)

