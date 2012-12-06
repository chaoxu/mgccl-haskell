import Rosalind
import Data.Function
import Data.Array
main :: IO ()
main  = do c <- getLine
           print $  wrightFisherModel (map read (words c) :: [Integer])
  where wrightFisherModel [n,m,g,k] = a!(2*n-m,g)
          where a = array ((0,0),(2*n,g)) [((i,j),d i j)|i<-[0..2*n],j<-[0..g]]
                d remain gen
                  | gen == 0    =   if remain < k then 0 else 1
                  | otherwise   =   sum [exact j * a!(j,gen-1) |j<-[0..2*n]]
                  where p = ((/) `on` fromIntegral) remain (2*n)
                        exact = binomial (2*n) p
