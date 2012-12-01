import Data.List
import Data.Array
main :: IO ()
main  = do s <- getLine
           t <- getLine 
           print $ lcs s t
lcs :: Eq a => [a] -> [a] -> [a]
lcs s t = reverse $ build n m
  where a = array ((0,0),(n,m)) [((x,y),f x y)|x<-[0..n],y<-[0..m]]
        n = length s
        m = length t
        f :: Int->Int->Int
        f i j
          | min i j == 0        = 0
          | s!!(i-1)==t!!(j-1)  = a!(i-1,j-1) +1
          | otherwise           = max (a!(i-1,j)) (a!(i,j-1))
        build i j 
          | min i j == 0          = []
          | s!!(i-1) == t!!(j-1)  = (s!!(i-1)):build (i-1) (j-1)
          | a!(i,j-1) > a!(i-1,j) = build i (j-1)
          | otherwise             = build (i-1) j
