module InverseFactoring where 

import Control.Applicative
getTheNumber :: [Int] -> Int
getTheNumber = liftA2 (*) maximum minimum
