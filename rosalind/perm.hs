import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           let t = (read c::Int)
           putStrLn $ show $ foldr1 (*) [1..t] 
           putStrLn $ show (perm t)

perm i = permute [1..i]
permute [] = [[]]
permute x = concat $ map (\y-> map (y:) (permute (delete y x))) x
