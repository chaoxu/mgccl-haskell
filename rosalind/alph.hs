import System.IO
import Rosalind
import Newick
import Data.Maybe
import Data.Tree
import Data.List
main :: IO ()
main  = do c <- getLine
           h' <- loop []
           let t = newickTree c
               h = map (build' . solve t . zip (map fst h')) $  transpose (map snd h')
               s = show $ sum (map fst h)
               z = map fst $ snd $ head h
               h''= s:zipWith (\x y-> ">"++x++"\n"++y) z (transpose (map (map snd.snd) h))
           putStr $ unlines h''
    where solve (Node (w,_) st) h
           | null st   = Node (w, leaf) []
           | otherwise = Node (w, best) f
           where f = map (`solve` h) st
                 z = map extract f
                 leaf = (l,0):map (\x->(x,9999)) (alpha \\ [l]) 
                 l = fromJust $ lookup w h
                 best = map dis alpha
                   where dis x
                          | length z == 1 = (x,minimum $ map diff left)
                          | otherwise     = (x,minimum [ diff a + diff b |a<-left,b<-right])
                          where left = head z
                                right = last z
                                diff (y,v) = if y/=x then v+1 else v

build' :: Tree (String, [(Char,Int)]) -> (Int,[(String,Char)])
build' n@(Node (a,b) st) = (v,build n c)
    where (v,c) = minimum [(y,x)|(x,y)<-b]
build :: Tree (String, [(Char,Int)]) -> Char -> [(String,Char)]
build (Node (a,b) st) c
 | null st       = []
 | length st == 1 = (a,c):best left
 | otherwise     = (a,c):pick left right
 where left = head st
       right = last st
       v = fromJust (lookup c b)
       best (Node (x,y) s) = build (Node (x,y) s) t
         where t = head [ o | (o,p)<-y, if o/=c then p+1==v else p == v]
       pick (Node (x1,y1) s1) (Node (x2,y2) s2) = build (Node (x1,y1) s1) t1 ++ build (Node (x2,y2) s2) t2
         where (t1,t2) = head [ (o1,o2) | (o1,p1)<-y1, (o2,p2)<-y2, helper o1 o2 p1 p2]
               helper o1 o2 p1 p2 = (diff o1 + diff o2 )+p1+p2 == v
               diff o1 = if o1/=c then 1 else 0
label   (Node x _) = x
extract (Node (_,xs) _) = xs
alpha = "ACGT-"

loop xs = do end <- isEOF
             if end
               then return xs
               else do c <- readFASTA
                       loop (c:xs)
