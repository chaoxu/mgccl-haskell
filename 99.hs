--99 problems in Haskell
import Data.List (sortBy)
--1
myLast = last

--2
myButLast = last . init

--3
elementAt = (last .) . (flip take)

--4
myLength = length

--5
--reverse itself

--6
isPalindrome xs = xs == reverse xs

--7

--8
compress (x:y:xs) | x==y      = compress (x:xs)
                  | otherwise = x:compress (y:xs)
compress xs = xs

--9
pack xs = reverse (pack' xs [])
pack' (x:xs) [] = pack' xs [[x]]
pack' [] zs = zs
pack' (x:xs) (z:zs) | x==head(z)      = pack' xs ((x:z):zs)
                    | otherwise = pack' xs ([x]:z:zs)

--10
encode xs = zip (map length (pack xs)) (compress xs)

--11

--12

--13

--14
dupli [] = []
dupli (x:xs) = x:x:dupli xs

--15
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

--16
dropEvery [] _ = []
dropEvery xs n = (init (take n xs)) ++ dropEvery (drop n xs) n

--17
split xs n = (take n xs, drop n xs)

--18
slice xs n m = take (m-n+1) (drop (n-1) xs) 

--19
rotate xs n = rotate' xs (rem ((rem n l) + l) l)
            where l = length xs
rotate' xs n = (drop n xs)++(take n xs)

--20
removeAt xs n = ((elementAt xs n), (take (n-1) xs)++(drop n xs))

--21
insertAt x xs n = (take (n-1) xs)++[x]++(drop (n-1) xs)

--22
range a b = [a..b]

--23

--26
combinations n [] =[[]]
combinations 0 _ = [[]]
combinations n (x:xs) = filter leneq ((map (x:) (combinations (n-1) xs)) ++ (combinations n xs))
                        where leneq x = (length x) == n

--27

--28 a)
sortLength a b 
  | length a < length b = LT
  | length a > length b = GT
  | length a == length b = compare a b
lsort ::Ord a0 => [[a0]]->[[a0]]
lsort = sortBy sortLength 

--28 b), similar to pack
