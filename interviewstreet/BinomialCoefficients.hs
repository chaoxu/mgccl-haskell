import Data.List
import Control.Monad
main = do
  n <- getInteger
  go 0 n

go i n =
  when (i < n) $
    do z <- getIntegerList
       print (test z)
       go (i + 1) n

getInteger = do
    l<-getIntegerList
    return (head l)

getIntegerList = do
    line <- getLine
    let tmp = words line
    return (map read tmp :: [Integer])

test [n,p] = 1 + n - product (map (+1) (digits p n))

digits p 0 = []
digits p n = digits p q ++[r]
  where (q,r) = quotRem n p
