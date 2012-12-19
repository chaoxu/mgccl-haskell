import Newick
import Data.Set hiding (map, filter, null)
import Data.Tree
import Data.Maybe

main :: IO ()
main = do c<-getLine
          t'<-getLine
          s'<-getLine
          let w = words c
              t = newickNameTree t'
              s = newickNameTree s'
          print $ 2* ((z $ toInteger $ length w)-(solve w t s))

z n = n*(n-1)*(n-2)*(n-3) `div` 24

solve w t' s' = sum [setCompute t s i j|i<-[1..length w],j<-[1..length w], i<j] `div` 2
    where a = zip ("":w) [0..]
          t = leafTree $ fmap (\x->fromJust $ lookup x a) t'
          s = leafTree $ fmap (\x->fromJust $ lookup x a) s'
treeMap f a@(Node x st) = Node (f a) (map (treeMap f) st)
leafTree = treeMap (fromList . leafLabels)

setCompute t s a b = sum [pairCount x y|x<-tl,y<-sl]
    where tl = setSearch t a b
          sl = setSearch s a b
setSearch t a b = filter (\x->size x>1) $ l:(concatMap (\x->if a `member` (rootLabel x) then search a x else search b x) $ subForest mainTree)
  where mainTree = fromJust $ searchTop a b t
        l = (rootLabel t) `difference` rootLabel mainTree
        searchTop :: Int -> Int -> Tree (Set Int) -> Maybe (Tree (Set Int))
        searchTop a b (v@(Node x st)) 
          | a `notMember` x || b `notMember` x = Nothing
          | a `member` x && b `member` x = head $ filter isJust $ map (searchTop a b) st ++ [Just v]
        search x (v@(Node xs st))
          | x `notMember` xs = [xs]
          | otherwise        = concatMap (search x) st

pairCount a b = n*(n-1) `div` 2
    where n = toInteger $ size $ a `intersection` b
