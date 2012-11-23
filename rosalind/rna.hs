import Data.List.Utils
main :: IO ()
main  = do c <- getLine
           putStrLn $ show (replace "T" "U" c)
