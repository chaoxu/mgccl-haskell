import Data.List
import qualified Data.Set as Set

main = do
  line <- getIntegerList
  line2 <- getIntegerList
  putStrLn (show $ test line2 (last line))

getIntegerList = do
    line <- getLine
    let tmp = words line
    return (map read tmp :: [Integer])

--test u k = length $ filter (not) $ map try divisors
test :: [Integer] -> Integer -> Int
test u k = length list
  where try t = or $ map (t `divides` ) list
        divisors = concat [ [i,div k i] | i<-[1..floor $ sqrt $ fromIntegral k], i `divides` k]
        --list = (nubOrd $ map (gcd k) u)
        list = (nubOrd $ map (gcd k) u)
d `divides` n = n `rem` d == 0

--snub = map head . group . sort

nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

primes = sieve [2..]
	where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
	      notdiv p n = n `mod` p /= 0
factorize n | n > 1 = go n (primes)    -- or: primes
   where
     go n ds@(d:t)
        | d*d > n    = [n]
        | r == 0     =  d : go q ds
        | otherwise  =      go n t
            where  (q,r) = quotRem n d
primePowers n
  | n > 1 = map (\x-> (head x, length x)) 
                       . group . factorize $ n
divisors n =  map product . sequence 
               . map (\(p,n)-> take (n+1) $ iterate (*p) 1)
               . primePowers $ n
