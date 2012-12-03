import Data.Array
main :: IO ()
main  = do c <- getLine
           putStrLn $ init $ spaceList $ kmp c
spaceList [] = []
spaceList (x:xs) = show x++" "++spaceList xs
kmp s = b
    where a = listArray (0,length s-1) s
          b = 0:list 0 (tail s)
          list _ [] = [] 
          list n (x:xs) 
            | x==a!n    = (n+1):list (n+1) xs
            | n > 0     = list (b!!(n-1)) (x:xs)
            | otherwise = 0:list 0 xs
