import System.IO
import Rosalind
import Data.List
import Data.Maybe
import Data.Graph.Inductive
import Data.Array
import Data.Ord
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStrLn  $ solve $ sort $ reverse xs
               else do c <- getLine
                       loop ((read c::Double):xs)

solve xs = pathEdgeLabel g $ longestPathDAG g
    where g = mkGraph v e :: Gr Double Char
          v = zip [0..n-1] xs
          e = [(i,j,fromJust $ proteinFromMass (b-a)) | 
                    (i,a)<-v,(j,b)<-v,isJust $ proteinFromMass (b-a)]
          n = length xs


pathEdgeLabel g xs = map fromJust $ zipWith (edgeLabel g) (init xs) (tail xs)
edgeLabel :: (Graph gr)=>gr a b->Node->Node->Maybe b
edgeLabel g i j
  | i `elem` pre g j = Just l
  | otherwise  = Nothing
    where (_,_,l) = head $ filter (\(a,b,_)->a==i&&b==j) (out g i)

