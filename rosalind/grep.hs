import System.IO
import Data.List
import Data.Maybe
import Data.Graph.Inductive
import Data.Set (fromList, toList)
main :: IO ()
main  = loop []
loop xs = do end <- isEOF
             if end
               then putStr $ unlines $ (toList.fromList) $ hamiltonianCyclesLabel $ deBruijnFromKmer $ reverse xs
               else do c <- getLine
                       loop (c:xs)
deBruijnFromKmer :: [String]->Gr Char ()
deBruijnFromKmer xs = mkGraph vertices edges
    where vertices = zip [0..n - 1] (map head xs)
          v        = zip [0..n - 1] xs
          edges    = [ (i,j,()) |(i,a)<-v,(j,b)<-v,i/=j,tail a==init b]
          n        = length xs

hamiltonianCyclesLabel g = [map (fromJust . lab g) c | c<-hamiltonianCycles g]
hamiltonianCycles :: (Graph gr)=> gr a b -> [[Node]]
hamiltonianCycles g = map reverse $ h [s]
        where s = head (nodes g) 
        --h input a list of used, current node.
              h x@(y:_)
                | length x == n && s `elem` suc g y = [x]
                | otherwise = concat [h (t:x)| t<-cand]
                where cand = suc g y \\ x
              n = length (nodes g)
