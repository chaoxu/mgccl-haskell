import System.IO
import Data.List
import Data.Function
import Data.Set hiding (map, filter)

main :: IO ()
main  = loop []
loop xs = do end <- isEOF
             if end
              then print result
              else do x <- getLine
                      loop (x:xs)
    where candidate = reverse $ sortBy (compare `on` length) $ toList $ fromList $ concatMap tails $ inits $ head xs
          result = head $ filter inAll candidate
          inAll s = all (s `isInfixOf`) (tail xs)
