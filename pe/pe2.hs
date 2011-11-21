fib = 0 : 1 : zipWith (+) fib (tail fib)
result = sum $ takeWhile (<4000000) (filter even fibs)
