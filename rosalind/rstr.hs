import Data.Maybe
main :: IO ()
main  = do d <- getLine
           c <- getLine
           let z = words d 
               a = read $ head z :: Int
               b = read $ last z :: Double
           print $ result b a c
    where prob x = zip "ATGC" $ [(1-x)/2,(1-x)/2,x/2,x/2]
          test x dna = product $ map (\y-> fromJust $ lookup y (prob x)) dna
          result x a dna = 1- ((1-(test x dna))^a)
