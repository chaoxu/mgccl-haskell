main :: IO ()
main  = do c <- getLine
           print $ foldl1 (\x y-> x*y `rem` 1000000) $ (\[n,k]->[n,n-1..n-k+1]) (map read (words c) ::[Integer])
