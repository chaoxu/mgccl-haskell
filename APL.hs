--This suppose to implement some APL functions, but we will only work with lists
--Which means everything is a 1D vector(implemented as list), anything above will be ignored.
import Data.List

--Reduce : ×/2 3 4 is the same as reduce (*) [2,3,4]
reduce :: (a -> a -> a) -> [a] -> a
reduce = foldr1

--n-wise Reduce : 2 ×/2 3 4 is the same as nWiseReduce 2 (*) [2,3,4]
nWiseReduce :: Int -> (a -> a -> a) -> [a] -> [a]
nWiseReduce n f xs = map (foldr1 f) (take (length xs-n+1) (map (take n) (tails xs)))

--Grade up ⍋:
gradeUp :: Ord a => [a] -> [Int]
gradeUp xs = snd $ unzip . sort $ zip xs [0..length xs-1]
--Grade Down ⍒:
gradeDown :: Ord a => [a] -> [Int]
gradeDown = reverse . gradeUp
--Index Function ⌷:
index :: [Int] -> [a] -> [a]
index is xs = [xs!!i|i<-is]
--Outer Product a∘.+b = outerProduct (+) a b
outerProduct :: (a->b->c) -> [a] -> [b] -> [[c]]
outerProduct f x y = [ [ f a b |b<-y] | a<-x]
--Inner Product a+.×b = innerProduct (+) (*) a b
innerProduct :: (c->c->c) -> (a->b->c) -> [a] -> [b] -> c
innerProduct f g x y = foldr1 f (zipWith g x y)
