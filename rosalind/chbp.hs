import System.IO
import Data.Graph.Inductive
import Data.Maybe
import Newick
import Data.Tree
import Data.List
import Rosalind
import Data.Set hiding (map, filter, null, foldl)
main :: IO ()
main  = do c <- getLine
           t' <-loop []
           let w = words c
               t = fromJust $ normalCharacterTable t'
               o = g w $ map (toSet w) t
               p = build o $ head $ topsort o
           putStrLn $ printNewickTree $ rootedToUnrooted p

toSet l t  = fromList [ x | (x,y) <-zip l t , y=='1']
g l t = grev $ gmap (sub gr) gr
  where gr = mkGraph v e :: Gr (Set String) ()
        v = zip [0..length t - 1] t
        n = length l
        e = map (\(x,y)->(x,y,())) $ filter (multi e2) e2
        e2 = [(y,x)|(x,a)<-v,(y,b)<-v, b `isProperSubsetOf` a, a/=b]
        sub g (p,i,a,s)
         | null pp    = (p,i,a,s)
         | otherwise = (p,i,a `difference` t,s)
         where t = unions $ map (fromJust . lab g ) pp
               pp = pre g i
        multi t (x,y)  = null $ startx `intersect` endy
          where startx = map snd $ filter ((== x).fst) t
                endy   = map fst $ filter ((== y).snd) t 

build g i
  | null s        = buildLeaf a
  | length s == 1 = Node "" [buildLeaf a, build g (head s)]
  | length s == 2 = Node "" [build g (head s),build g (last s)]
 where s = suc g i
       a = toList $ fromJust $ lab g i
       buildLeaf [a,b] = Node "" [Node a [], Node b []]
       buildLeaf [a]   = Node a []
rootedToUnrooted (Node a [Node b x,y]) = Node b (y:x)
loop xs = do end <- isEOF
             if end
               then return $ reverse xs
               else do c <- getLine
                       loop (c:xs)
