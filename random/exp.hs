import Data.Digits
exponentiationBySquaring op a n = foldr1 op $ [y | (x,y) <- (zip binary twoPow), x/=0]
  where twoPow = a:zipWith op twoPow twoPow
        binary = digitsRev 2 n
