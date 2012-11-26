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
--Current element and the path to current element
type Perm = [Int]
data Path = Path Perm [(Int,Int)]
notMember h x = not $ member x h
data Solution = Sol Path | Cand [Path]
instance Show Solution where
    show (Sol n) = show n
    show (Cand n) = "Cand"++show n
instance Show Path where
    show (Path x y) = "{"++show x ++ show y++"}"
--bfs :: Perm -> [Perm]
--solve::[Int]->[Int]->[Perm]
--solve a b = bfs c
solve a b
  | a == b    = []
  | otherwise = bfs c
    where c = applyPermutation (inversePermutation $ map dec b) $ map dec a
          dec x = x-1
bfs start = p $ head $ dropWhile (done.fst) $ iterate candidate (Cand [Path start []],empty)
    where sol = [0..length start-1]
          done (Sol _) = False
          done _ = True
          candidate (Cand (Path x y:xs),h)
            | sol == x = (Sol (Path sol y),newh)
            | otherwise  = (Cand (xs++[Path t (s:y) |(s,t)<-ls]),newh)
            where newh = h `union` fromList l
                  c = breakCount x
                  l = map snd ls
                  ls = filter (\x-> c>breakCount (snd x)) $ filter (notMember h.snd) [((i+1,j),rev i j x)|i<-[0..n-2],j<-[i+2..n]]
                  n = length x
          p (Sol (Path _ y),_) = y
