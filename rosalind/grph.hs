import System.IO
import Rosalind

main :: IO ()
main  = loop []
loop xs = do end <- isEOF
             if end
              then print result
              else do c<-readFASTA
                      loop (c:xs)

    where result = concatMap edges vert
          vert   = map tran xs
          tran (a,b) = (a,take 3 b, drop (length b-3) b)
          edges (x,_,y) = zip (replicate (length adj) x) adj
            where adj = filter (/=x) $ map (\(a,z,_)-> if y==z then a else x) vert
