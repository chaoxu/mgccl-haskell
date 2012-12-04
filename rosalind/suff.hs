import Data.SuffixTree
import Data.List
main :: IO ()
main  = do c <- getLine
           putStr $ concat [x++"\n"|x<-printTree $ buildTree c]
buildTree = constructWith "ACGT$" 
printTree (Node a) = (map (prefix.fst) a)++(concatMap (printTree.snd) a)
printTree  Leaf    = []
