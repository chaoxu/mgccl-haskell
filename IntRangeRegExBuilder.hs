import Data.Digits
-- build regular expression for positive integer ranges
-- in base 10. The program uses operations [x-y],|,\d,*,()

data RegEx = Range Int Int | MatchNone | All Int | Star
            | Or RegEx RegEx | Concat [RegEx] 

-- You could also use "[0-9]" if you like
alphabet = "\\d"

instance Show RegEx where
  show (Range i j)
   | i == j    = show i
   | otherwise = concat ["[",show i,"-",show j,"]"]
  show (Or a b)  = show a ++ "|" ++ show b
  show MatchNone = "^$"
  show Star = "*"
  show (All n) = concat $ replicate n alphabet
  show e@(Concat xs) 
   | atomic e  = concatMap show xs
   | otherwise = concatMap show' xs
   where show' (Or a b) = "("++show (Or a b)++")"
         show' x = show x
         atomic (Concat xs) = all atomic xs
         atomic (Or _ _)    = False
         atomic _           = True

-- build the regular expression
buildRegEx :: RegEx->String
buildRegEx = show

-- Match leading zeros
matchLeadingZeros :: RegEx->RegEx
matchLeadingZeros x = Concat [Range 0 0, Star, x]

-- Match integers in a certain range
matchIntRange :: Int->Int->RegEx
matchIntRange a b
 | 0 > min a b = error "Negative input"
 | a > b       = MatchNone
 | otherwise = build (d a) (d b)
 where build :: [Int]->[Int]->RegEx
       build [] [] = Concat [] 
       build (a@(x:xs)) (b@(y:ys))
         | sl && x == y  = Concat [Range x x, build xs ys]
         | sl && all9 ys = Concat [Range x y, All n]
         | sl            = Or (Concat [Range x (y-1), All n]) (build (y:r 0 n) b)
         | otherwise     = Or (build a (r 9 la)) (build (1:r 0 la) b)
         where (la,lb) = (length a, length b)
               sl      = la == lb
               n       = length xs
               all9    = (==r 9 n)
       r k n   = replicate n k
       d 0 = [0]
       d n = digits 10 n

matchLessThan :: Int->RegEx
matchLessThan = matchIntRange 0

matchGreaterThan :: Int->RegEx
matchGreaterThan a = Or (matchIntRange a b) (Concat [Range 1 9, All n, Star])
  where n = length $ digits 10 a
        b = unDigits 10 $ replicate n 9
