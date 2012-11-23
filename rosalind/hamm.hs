import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           d <- getLine
           putStrLn $ show (hammDis d c)
hammDis a b = sum $ zipWith (\x y->if (x==y) then 0 else 1) a b
