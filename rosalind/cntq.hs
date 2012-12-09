main :: IO ()
main  = do c <- getLine
           print $ solve (read c :: Integer)
    where solve n = (product [n-3..n] `div` 24) `rem` 1000000
