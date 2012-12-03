import System.IO
import Data.Graph.Wrapper
import Data.List


main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ result xs
               else do c <- getLine
                       loop (c:xs)
  where result xs = foldl1' connect $ topologicalSort
                    $ fromVerticesEdges (zip xs (repeat 0)) [(x,y)|x<-xs,y<-xs,x/=y,e x y]
          where t = (length xs `div` 2)+1 
                e x y = not $ null [True|a<-f (tails x),b<-f (inits y),a==b]
                f = filter (\y->length y >= t)
                connect xs ys = (take pos xs)++ys
                  where pos = head $ elemIndices True $ map (\x->x ys) pre
                        pre = map isPrefixOf (tails xs)
