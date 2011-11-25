--Find the square root of a integer without using built
--in square root function
--One solution: Using trigonometry and Lagrange's four-square theorem

sqrtOfInteger n = magnitude $ filter (0/=) vector
  where vector = head $ dropWhile (((/=) n) . sum . (map (^2)) ) (latticePoints 4)

--generate all lattice points in dimension k
latticePoints k = concat $ map (sumToN k) [0..]
sumToN k n | k == 1    = [[n]]
           | otherwise = concat [(map (i:) (sumToN (k-1) (n-i))) | i<-[0..n]]

--find magnitude of a vector using trig. where non of the coordinate is 0
magnitude [v] = abs v
magnitude (u:v:vs) = abs $ u / (sin (atan (u/magnitude (v:vs))))
