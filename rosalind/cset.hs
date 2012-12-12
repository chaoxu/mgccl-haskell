import System.IO
import Data.Maybe
import Rosalind
main :: IO ()
main  = do t <-loop []
           putStr $ unlines $ solve t

solve xs = remove v
    where remove k = [xs!!x|x<-[0..n-1],x/=k]
          n = length xs
          v = fst $ head $ dropWhile (isNothing.snd) $ map (\x-> (x,normalCharacterTable $ remove x )) [0.. n-1]

loop xs = do end <- isEOF
             if end
               then return $ reverse xs
               else do c <- getLine
                       loop (c:xs)
