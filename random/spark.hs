-- Generate a sparkline, Haskell version of <https://github.com/holman/spark/>
-- Usage: after compile, try '$ spark -10 0 15.24 42'
-- Author: Chao Xu, <http://chaoxuprime.com>
import System (getArgs)

main :: IO ()
main = do input <- getArgs
          putStrLn $ spark (map read input :: [Double])

spark :: RealFrac b => [b] -> String
spark list = map ("▁▂▃▄▅▆▇" !!) xs
           where zs = map (flip (-) (minimum list)) list
                 xs = map (round . (* 6) . (/ maximum zs)) zs