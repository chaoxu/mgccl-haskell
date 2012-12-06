import Data.Maybe
main :: IO ()
main  = do n2 <- getLine
           c <- getLine
           d <- getLine
           let z = map read (words d) :: [Double]
               n = read n2 :: Int
           putStrLn $ unwords $ map (\x->show $ fromIntegral (n-length c+1) * test x c) z
    where prob x = zip "ATGC" [(1-x)/2,(1-x)/2,x/2,x/2]
          test x dna = product $ map (\y-> fromJust $ lookup y (prob x)) dna
          
