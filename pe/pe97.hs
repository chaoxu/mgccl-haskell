bin 0 xs = xs
bin n xs = bin q (r:xs)
        where (q,r) = divMod n 2
result = (t*28433+1) `mod` m
  where t      = foldr (mmod) 1 $ filter (/=0) (zipWith (*) (reverse $ bin 7830457 []) twoPow)
        twoPow = 2:4:zipWith mmod (tail twoPow) (tail twoPow)
        m      = 10^10
        mmod a b = mod (a*b) m
