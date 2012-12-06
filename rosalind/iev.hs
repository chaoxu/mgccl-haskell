main :: IO ()
main  = do c <- getLine
           print $ (* 2) $ sum $ zipWith (*) [1,1,1,0.75,0.5,0] (map read (words c) ::[Double])
