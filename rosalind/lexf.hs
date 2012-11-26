import Data.List
import Control.Monad
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let t = read c::String
               n = read d::Int
           print $ replicateM n t
