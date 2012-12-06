import Math.Combinatorics.Exact.Binomial

main :: IO ()
main  = do c <- getLine
           print $ (\[x,y]->binomial (2^x) y) (map read (words c) ::[Integer])
           
  where binomial n m = sum [(fromIntegral (n `choose` k))*0.25^k*0.75^(n-k)|k<-[m..n]]
