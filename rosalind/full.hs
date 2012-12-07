import System.IO
import Rosalind
import Data.List
import Data.Maybe
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStrLn $ solve $ tail $ reverse xs
               else do c <- getLine
                       loop ((read c::Double):xs)

solve xs = findword (head p) (tail p)
        where p = zip (take t z) (take t $ reverse z)
              z = sort xs
              t = length z `div` 2
              findword _ []   = ""
              findword x list = best:findword (o,p) list'
                where cand = list ++ [(d,c)|(c,d)<-list]
                      ((_,best),(o,p)) = minimum $ map (match x) cand
                      match (a,b) (c,d) = (proteinFromMassError (c-a),(c,d))
                      list' = list \\ [(o,p),(p,o)]
