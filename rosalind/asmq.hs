import System.IO
import Data.List
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print (nstat $ ceiling $ t * 0.5, nstat $ ceiling $ t * 0.75)
               else do c <- getLine
                       loop (length c:xs)
    where l = zip (scanl1 (+) ys) ys
          ys = reverse $ sort xs
          t = fromIntegral $ sum xs
          nstat k = snd $ head $ filter ((<=) k.fst) l
