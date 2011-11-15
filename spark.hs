import System( getArgs )
--Generate sparkline, haskell version of https://github.com/holman/spark/
--Chao Xu, http://chaoxuprime.com
main = do
  args <- getArgs
  let input = args
  putStrLn $ spark (map read input :: [Double])

ticks = "▁▂▃▅▆▇"
spark list = map (ticks!!) xs
           where zs = map (flip (-) (minimum list)) list
                 xs = map (round . (*5) . (/(maximum zs))) zs
