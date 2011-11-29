import Data.List

--Lazy linear homogeneous recurrence relations with constant coefficient
linearRecurrence coef term = base ++ map (sum . (zipWith (*) coef)) (map (take n) (tails a))
  where a = linearRecurrence coef term
        base = replicate (n - (length bterm)) 0 ++ bterm
        bterm = (take n term)
        n = (length coef)

--Generalize the above to linear homogeneous recurrence relation in any field
--Note the zero for * should be the zero for +.
generalLinearRecurrence coef term mulOperator plusOperator zero = 
  base ++ map ((foldr plusOperator zero) . (zipWith mulOperator coef)) (map (take n) (tails a))
  where a = generalLinearRecurrence coef term mulOperator plusOperator zero
        base = replicate (n - (length bterm)) zero ++ bterm
        bterm = (take n term)
        n = (length coef)
