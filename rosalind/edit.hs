import Data.List
import Data.Array
main :: IO ()
main  = do s <- getLine
           t <- getLine 
           print $ editDistance s t
editDistance s t = a!(n,m)
  where a = array ((0,0),(n,m)) [((x,y),f x y)|x<-[0..n],y<-[0..m]]
        n = length s
        m = length t
        f i j
          | min i j == 0        = max i j
          | otherwise           = minimum [x,y,z]
          where x = 1+a!(i-1,j)
                y = 1+a!(i,j-1)
                z = (a!(i-1,j-1))+(if s!!(i-1)==t!!(j-1) then 0 else 1)
