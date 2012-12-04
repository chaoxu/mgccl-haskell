import System.IO
import Data.List
import Data.Ord
import Data.Graph.Wrapper
data Tree = Node Int String [Tree] deriving Show
main :: IO ()
main  = do s <- getLine
           n1 <- getLine
           pr <- loop s []
           let n = read n1 ::Int
           print $ solve' n s $ buildTree n (buildGraph pr) 0
buildGraph xs = fromVerticesEdges vertices edges
  where edges    = map fst xs
        vertices = (0,""):map (\((_,x),y)->(x,y)) xs
buildTree n g i
  | null subTrees = Node 1 (vertex g i) []
  | otherwise     = Node (count subTrees) (vertex g i) (filter (\(Node t _ _)->t>=n) subTrees)
  where subTrees = map (buildTree n g) (successors g i)
        count xs = sum $ map (\(Node t _ _)->t) xs
solve' n dna = solve
  where solve (Node x s xs)
         | null xs   = s
         | otherwise = s ++ maximumBy (comparing length) (map solve xs)
loop c xs = do end <- isEOF
               if end
                 then return $ sort xs
                 else do s <- getLine
                         loop c (parseEdge c s:xs)
parseEdge dna xs = ((x-1,y-1),take b $ drop (a-1) dna)
  where d = words xs
        x = read $ drop 4 $ head d   :: Int
        y = read $ drop 4 $ head (tail d)  :: Int
        a = read $ d!!2 :: Int
        b = read $ d!!3 :: Int
