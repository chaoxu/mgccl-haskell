main :: IO ()
main  = do n <- getLine
           print $ 2^(read n::Int) `rem` 1000000
