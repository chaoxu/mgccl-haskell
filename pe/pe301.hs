fib = 0:1:zipWith (+) fib (tail fib)
result = fib!!32
