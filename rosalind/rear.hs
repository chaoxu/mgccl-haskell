{-# LANGUAGE FlexibleInstances #-}
import Data.List hiding (union)
import Data.Hashable() 
import Data.HashSet hiding (map, filter)
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let b = read d :: [Int]
               a = read c :: [Int]
               z = reverse $ solve a b
           putStrLn $ show (length z) ++ show z

breakCount xs = h + l + sum (zipWith (\x y-> if 1 == abs (x-y) then 0 else 1) (tail xs) xs)
  where h = if 0==head xs then 0 else 1
        l = if length xs - 1==last xs then 0 else 1
inversePermutation x = map snd $ sort [(x!!i,i)|i<-[0..9]]
applyPermutation p x = [p!!(x!!i) | i<-[0..9]]
rev i j list = take i list ++ drop (n-j) (reverse (drop i list))++ drop j list
  where n = length list
data Path = Path [Int] [(Int,Int)]
notMember h x = not $ member x h
solve a b
  | a == b    = []
  | otherwise = bfs c
    where c = applyPermutation (inversePermutation $ map dec b) $ map dec a
          dec x = x-1
bfs start = p $ head $ fst $ head $ dropWhile (done.fst) $ iterate candidate ([Path start []],empty)
    where sol = [0..length start-1]
          done (Path x y:ys) 
            |x == sol = False
            |otherwise = True
          candidate (Path x y:ps,h)= (ps++[Path t (s:y) |(s,t)<-ls],newh)
            where newh = h `union` fromList (map snd ls)
                  ls = filter (\y-> breakCount x>breakCount (snd y)) $ filter (notMember h.snd) [((i+1,j),rev i j x)|i<-[0..n-2],j<-[i+2..n]]
                  n = length x
          p (Path _ y) = y

instance Show Path where
    show (Path x y) = "{"++show x ++ show y++"}"
