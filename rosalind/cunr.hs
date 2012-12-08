main :: IO ()
main  = do c <- getLine
           print $ doubleFactorial (2*(read c::Int)-5)
    where doubleFactorial n = foldl1 (\x y->x*y `rem` 1000000) [n,n-2..1]
