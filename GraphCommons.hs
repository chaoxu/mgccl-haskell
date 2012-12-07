import Data.Maybe
import Data.Graph.Inductive
import Data.List
import Data.Array
import Data.Ord 
hamiltonianCycles :: (Graph gr)=> gr a b -> [[Node]]
hamiltonianCycles g = map reverse $ h [s]
        where s = head (nodes g) 
        --h input a list of used, current node.
              h x@(y:_)
                | length x == n && s `elem` suc g y = [x]
                | otherwise = concat [h (t:x)| t<-cand]
                where cand = suc g y \\ x
              n = length (nodes g)

hamiltonianCycle :: (Graph gr)=> gr a b -> Maybe [Node]
hamiltonianCycle g
  | null c    = Nothing
  | otherwise = Just (head c)
  where c = hamiltonianCycles g

longestPathDAG :: (Graph gr)=>gr a b ->[Node]
longestPathDAG g = maximumBy (comparing length) $ elems a
  where a = array (0,n-1) [(i,d i)|i<-[0..n-1]]
        n = length (nodes g)
        d i 
          | null v    = [i]
          | otherwise = maximumBy (comparing length) [t i j|j<-v]
          where v = suc g i
                t i j
                  | j `elem` v = i:(a!j)
                  | otherwise  = [i]

pathEdgeLabel g xs = map fromJust $ zipWith (edgeLabel g) (init xs) (tail xs)
edgeLabel :: (Graph gr)=>gr a b->Node->Node->Maybe b
edgeLabel g i j
  | i `elem` pre g j = Just l
  | otherwise  = Nothing
    where (_,_,l) = head $ filter (\(a,b,_)->a==i&&b==j) (out g i)

