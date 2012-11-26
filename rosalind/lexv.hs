import Data.List
import Control.Monad
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let t = read c::String
               n = read d::Int
           print $ gen n t

gen 0 _ = [[]]
gen n xs = concat ([""]:map (\x-> map (x:) t) xs)
  where t = gen (n-1) xs
