import Math.Combinatorics.Exact.Binomial

main :: IO ()
main  = do c <- getLine
           print $ (\[x,y]->binsum x y) (map read (words c) ::[Integer])
    where binsum n m = foldl1 (\x y -> (x+y) `rem` 1000000) [n `choose` k|k<-[m..n]]
