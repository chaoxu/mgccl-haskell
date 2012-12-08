import Data.Maybe
import System.IO
import Newick
import Data.Tree
main :: IO ()
main  = loop []
loop xs = do c <- getLine
             d <- getLine
             let [x,y] = words d 
             do end <- isEOF
                if end
                then putStrLn $ unwords $ map show $ solve $ reverse ((c,x,y):xs)
                else do getLine
                        loop ((c,x,y):xs)
  where solve xs = map (\(c,x,y)->distance (newickTree c) x y) xs

distance t a b = inSubtree t
  where e = 10000000
        inSubtree s@(Node (n,len) xs)
          | null xs              = e
          | isJust x && isJust y = minimum $ (fromJust x+fromJust y-(2*len)):l
          | otherwise            = e
          where x = inside s a
                y = inside s b
                l = map inSubtree xs
                
        inside (Node (n,len) xs) x
          | x == n    = Just len
          | null list = Nothing
          | otherwise = Just $ len + fromJust y
          where list = filter (isJust) $ map (`inside` x) xs
                y    = head list
