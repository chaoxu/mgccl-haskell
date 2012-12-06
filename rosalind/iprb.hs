main :: IO ()
main  = do c <- getLine
           print $ dominate (map read (words c) ::[Double])
    where dominate [k,m,n] = 1 - ((n*n) + ((m*(m-1))* 0.25) + (m*(n-1)))/(t*(t-1))
            where t = k+m+n
