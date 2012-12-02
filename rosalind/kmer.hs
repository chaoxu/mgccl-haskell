import Data.List
import Control.Monad
import System.IO
main :: IO ()
main  = loop[] 

loop xs = do end <- isEOF
             if end
               then putStrLn $ show $ result xs
               else do c <- getLine
                       if head c == '>'
                         then loop xs
                         else loop (xs++c)

countSublist x y = length $ filter (isPrefixOf x) (tails y)
result xs = map (`countSublist` xs) (replicateM 4 "ACGT")

