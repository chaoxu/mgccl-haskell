import Data.List

--Lazy linear homogeneous recurrence relations with constant coefficient
linearRecurrence coef base | n /= (length base) = []
                           | otherwise = a
  where a = base ++ map (sum . (zipWith (*) coef)) (map (take n) (tails a))
        n = (length coef)
