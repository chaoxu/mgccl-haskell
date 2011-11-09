--Solutions for coding problems in The Haskell Road to Logic, Maths and Programming
import TUOLP
import List
import SetEq
--1.9
maxInt []     = error "empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)
--1.10
removeFst _ []  = []
removeFst m (x:xs) | x == m    = xs
                   | otherwise = x:(removeFst m xs)
--1.13
count _ [] = 0
count c (x:xs) | x == c    = 1+ z
               | otherwise = z
               where z = count c xs
--1.14
blowup a = blowup' a 1
blowup' (x:xs) k = take k (repeat x) ++ blowup' xs (k+1)
blowup' [] _ = []

--1.15
srtString :: [String] -> [String]
srtString xs | length xs > 0 = m:srtString (removeFst m xs)
             | otherwise     = []
             where m = foldr1 min xs

prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

--1.17
substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs (y:ys) = prefix xs (y:ys) || prefix xs ys

--1.20
lengths = map length

--1.21
sumLengths = sum . lengths

--useful
every xs p = all p xs
some xs p = any p xs
--2.51
unique _ [] = False
unique p (x:xs) | (p x)      = not (any p xs)
                | otherwise  = unique p xs
--2.52
parity [] = True
parity (x:xs) | x      = not (parity xs)
              | not x  = parity xs

--2.53
--evenNR p xs = (parity . (map p)) xs
evenNR = (parity .) . map

--3.38
fasterprimes = 2:sieve (map ((1+).(2*)) [ 1..]) 

--3.39
euclid = and (map (prime . (1+) . product) [take x primes | x<-[1..]])

--4.44
listOrder :: Ord a => [a] -> [a] -> Ordering
listOrder x y 
  | length x == length y  = compare x y
  | otherwise             = compare (length x) (length y)

--4.46
reverse' :: [a]->[a]
reverse' []     = []
reverse' (x:xs) = (reverse' xs)++[x]

--4.47
splitList :: [a] -> [([a],[a])]
splitList x = splitListAux x []
splitListAux [x] ys = []
splitListAux (x:xs) ys = (reverse (x:ys),xs):(splitListAux xs (x:ys))

--4.51
setDifference x y | rem == [] = x
                  | otherwise = foldr delete x rem
                  where rem = intersect x y

--4.53
getUnion [x] = x
getUnion x = foldr1 union x
getIntersect [x] = x
getIntersect x = foldr1 intersect x

--4.54
unionSet (Set []) xs = xs
unionSet xs (Set []) = xs
unionSet xs (Set (y:ys)) = unionSet (insertSet y xs) (Set ys)

intersectSet (Set []) _ = Set []
intersectSet _ (Set []) = Set []
intersectSet (Set (x:xs)) ys | inSet x ys = insertSet x (intersectSet (Set xs) ys)
                             | otherwise  = intersectSet (Set xs) ys

differenceSet (Set []) xs = Set []
differenceSet xs (Set []) = xs
differenceSet xs (Set (y:ys))  = differenceSet (deleteSet y xs) (Set ys)

--4.55
--doesn't the code already have that?
