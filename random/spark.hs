import System( getArgs )
--Generate sparkline, haskell version of https://github.com/holman/spark/
--usage, after compile, try spark -10 0 15.24 42
--Chao Xu, http://chaoxuprime.com
main = do
  args <- getArgs
  let input = args
  putStrLn $ spark (map read input :: [Double])

ticks = "▁▂▃▅▆▇"
spark list = map (ticks!!) xs
           where zs = map (flip (-) (minimum list)) list
                 xs = map (round . (*5) . (/(maximum zs))) zs
