import Math.Combinatorics.Exact.Binomial
import Data.Ratio

main :: IO ()
main  = do c <- getLine
           putStrLn $ unwords $ map show $ binomial $ 2*(read c :: Integer)
    where binomial n = map (logBase 10 . fromRational) list
             where list = map (\x->(1%1) - (x * ((1%2)^n))) $ scanl1 (+) [(n `choose` k)%1|k<-[0..n-1]]
