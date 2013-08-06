-- This solves the problem
-- http://mcicpc.cs.atu.edu/archives/2008/mcpc2008/serials/serials.html
-- It's also problem 4172 on ACM-ICPC Live Archive

import Data.Monoid
import StepFunction
import Data.List.Split

main :: IO ()
main = interact process

data Code = Code String | Identity deriving (Eq, Show)

-- Notice the append is nothing more than always take the right value unless
-- it is Identity
instance Monoid Code where
  mempty = Identity
  mappend a b
   | a == Identity = b
   | b == Identity = a
   | otherwise     = b

process :: String -> String
process input = concatMap solve problem
  where problem = init $ splitOn ["0"] $ lines input
        -- solve each individual problem
        solve x = head x ++ "\n" ++ build
          where build = concatMap writeCommand $ (\(_,t,_)->t) $ toList $ stepFunction sol
                sol = map readCommand (tail x)

readCommand :: String -> (Int, Int, Code)
readCommand s = (read a, read b + 1, Code (unwords [c,d]))
 where [a,b,c,d] = words s

writeCommand :: (Int, Int, Code) -> String
writeCommand (a, b, Code c)   = show a ++ " " ++ show (b-1) ++ " " ++ c ++ "\n"
writeCommand (_, _, Identity) = ""

