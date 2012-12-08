import System.IO
import Data.Set (fromList, toList)
main :: IO ()
main  = do list <- getLine 
           p    <- loop []
           let l = words list
           putStr $ unlines $ map printQuartets $ (toList.fromList) $ concatMap (\x->quartets $ part x l ([],[])) p
  where part [] _ z = z
        part (x:p) (y:l) (zero,one)
          | x == '0'  = part p l (y:zero,one)
          | x == '1'  = part p l (zero,y:one)
          | otherwise = part p l (zero,one)
        quartets xs = [if x<y then (x,y) else (y,x)|x<-pairs (fst xs),y<-pairs (snd xs)]
        printQuartets ((a,b),(c,d)) = "{"++a++", "++b++"} {"++c++", "++d++"}"
        pairs xs = [(x,y)|x<-xs,y<-xs,x<y]

loop xs = do end <- isEOF
             if end
               then return xs
               else do c <- getLine 
                       loop (c:xs)
