main :: IO ()
main  = do c <- getLine
           print $ map (\x -> length $ filter (==x) c) "ACGT"
