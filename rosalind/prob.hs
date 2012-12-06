import Data.Maybe
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let z = map read (words d) ::[Double]
           putStrLn $ unwords $ map (show . (`test` c)) z
    where prob x = zip "ATGC" $ map (logBase 10) [(1-x)/2,(1-x)/2,x/2,x/2]
          test x dna = sum $ map (\y-> fromJust $ lookup y (prob x)) dna
