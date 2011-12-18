import Data.List
import Data.Maybe
eventuallyPeriodic :: Eq a => [a] -> Int -> ([a], [a])
eventuallyPeriodic sequence bound = (ini,take period rep)
  where table      = map (take (bound+1)) (tails (map (take bound) (tails sequence)))
        exist      = map (\x-> elemIndex (head x) (tail x)) table
        period     = 1 + (fromJust $ head just)
        (no, just) = span isNothing exist
        (ini, rep) = splitAt (length no) sequence
