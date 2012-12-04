import Data.Array
main :: IO ()
main  = do c <- getLine
           putStr $ init $ spaceList $ kmp c
spaceList [] = []
spaceList (x:xs) = show x++" "++spaceList xs
kmp s = elems b
    -- see this http://stackoverflow.com/q/13676616/303863
    -- originally I can't get it to O(n)
    where a = listArray (0,l-1) s
          b = listArray (1,l) (0:map (\q -> f (b!(q-1)) q) [2..l])
          f n q
           | n > 0 && (a ! n) /= (a ! (q-1)) =
           f (b ! n) q
           | a ! n == a ! (q-1) = n + 1
           | otherwise = n
          l = length s
