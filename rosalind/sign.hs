import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           let t = (read c::Int)
           putStrLn $ show $ foldr1 (*) [2,4..(2*t)] 
           putStrLn $ show (concat $ map sign (perm t))

perm i = permute [1..i]
permute [] = [[]]
permute x = concat $ map (\y-> map (y:) (permute (delete y x))) x

sign::Num a=> [a]->[[a]]
sign [] = [[]]
sign (x:xs) = front x ++ (front (-x))
  where front a = map (a:) (sign xs)
