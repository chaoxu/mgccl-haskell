import Data.Maybe
import Newick
import Data.Set
main :: IO ()
main = do getLine
          c <- getLine
          d <- getLine
          print $ splitDistance (newickTree c) (newickTree d)

splitDistance :: NewickTree -> NewickTree -> Int
splitDistance c d = (2*(n-3))- (2 * size ( fromList ( splits c) `intersection` fromList ( splits d)))
  where n = length $ leafLabels c
