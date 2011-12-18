import Data.List
rec :: Num a => [a] -> [a] -> [Int] -> [a]
rec c b m = a
  where a = c++rest
        rest = next [] 0 m
        next xs k (m:ms) | k == m    = next (a:xs) k ms
                         | otherwise = val ++ next (map tail xs) (k+1) (m:ms)
          where val = if (k<length c) then [] else [sum $ zipWith (*) (reverse (map head xs)) b]

pentagonalNumbers = [(3 * n^2 - n) `div` 2|n<-[1..]]

integers = (0:)$ concat $ zipWith (\x y -> [x,y]) [1..] (map negate [1..])

generalizedPentagonalNumbers = [(3 * n^2 - n) `div` 2|n<-integers]

partitionNumbers = rec [1] (cycle [1,1,-1,-1]) (tail generalizedPentagonalNumbers)
