import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           putStrLn $ unwords $ map show $ process $ sort (map read (words c) :: [Int])
process l = sort $ fromJust $ 
            build [0,last l] cand l
  where cands bound diff sol
          | length diff < 2 = sol
          | a + b > bound   = cands bound (init diff) sol
          | a + b < bound   = cands bound (tail diff) sol
          | otherwise       = cands bound ((init.tail) diff) ((a,b):sol)
          where a = head diff
                b = last diff
        cand = cands (last l) (init l) []
        n = (floor (sqrt $ fromIntegral (8*length l+1))+1) `div` 2
        build xs [] _
          | length xs /= n = Nothing
          | otherwise      = Just xs
        build xs ((a,b):cs) d
          | (length xs + length cs) < n - 1 = Nothing
          | otherwise                       = listToMaybe $ catMaybes [if isJust $ newd a then build (a:xs) cs (d' a) else Nothing,
                                                                       if isJust $ newd b then build (b:xs) cs (d' b) else Nothing,
                                                                       build xs cs d]
            where newd x = remove (sort (map (abs . (x - )) xs)) d []
                  d'   x  = fromJust (newd x)
        remove (x:xs) (y:ys) o
          | x == y = remove xs ys o
          | y > x  = Nothing
          | y < x  = remove (x:xs) ys (y:o)
        remove [] ys o = Just (reverse o++ys)
        remove (_:_) [] _ = Nothing

