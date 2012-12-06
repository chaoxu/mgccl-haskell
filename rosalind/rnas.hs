import Data.List
import Data.Array
main :: IO ()
main  = do c <- getLine
           print $ count c

count s = a!(0,n-1)
 where a = array ((0,0),(n-1,n-1)) [((i,j),d i j)|i<-[0..n-1],j<-[0..n-1]]
       n = length s
       d i j
        | j < i       = 0  
        | j - i < 4   = 1
        | otherwise   = (a!(i+1,j)) + e + sum (map prod cand)
        where prod k
               | i>=n-1    = 1
               | otherwise = (a!(i+1,k-1))*(a!(k+1,j))
              cand = filter (p i) [i+4..j-1]
              p x y = sort [s!!x,s!!y] `elem` ["AU","CG","GU"]
              e = if p i j then a!(i+1,j-1) else 0
