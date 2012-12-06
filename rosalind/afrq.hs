main :: IO ()
main  = do c <- getLine
           putStrLn $ unwords $ map (show . solve) (map read (words c) :: [Double])
    where solve c = c+2*(sqrt c-c)
