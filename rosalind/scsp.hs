import Rosalind
main :: IO ()
main  = do s <- getLine
           t <- getLine 
           putStrLn $ scs s t

scs s t = build (lcs s t) s t
    where build [] a b = a++b
          build (x:xs) a b = (a1++b1)++[x] ++ build xs (tail a2) (tail b2)
            where (a1,a2) = break (==x) a
                  (b1,b2) = break (==x) b
