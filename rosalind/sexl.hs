main :: IO ()
main  = do c <- getLine
           putStrLn $ unwords $ map (show . (\x->2*x*(1-x))) (map read (words c) :: [Double])
