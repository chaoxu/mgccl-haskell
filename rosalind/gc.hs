import System.IO
import Rosalind

main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ maximum [ (gcContent y,x) |(x,y)<-xs]
               else do c <- readFASTA
                       loop (c:xs)
    where gcContent l = (fromIntegral $ length $ filter (\y-> y=='C' || y=='G') l)/ (fromIntegral $ length l)

