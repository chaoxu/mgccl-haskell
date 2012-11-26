import Data.List
main :: IO ()
main  = do c <- getLine
           d <- getLine
           let a = read c::String
               b = read d::String
           print $ map (+1) $ findIndices (isPrefixOf b) $ tails a
