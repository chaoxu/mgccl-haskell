main :: IO ()
main  = do c <- getLine
           putStrLn $ show (countN c)

countN a = map (\x -> length $ filter (==x) a) ['A','C','G','T']
