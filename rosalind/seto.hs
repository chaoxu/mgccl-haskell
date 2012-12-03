import Data.Set hiding (map)

main :: IO ()
main  = do n <- getLine
           a <- getLine
           b <- getLine
           let u = fromList [1..(read n::Int)]
               x = fromList (read a::[Int])
               y = fromList (read b::[Int])
           putStrLn $ concatMap showSet $ map toList [x `union` y, x `intersection` y,x \\ y, y \\ x, u \\ x, u \\ y]
    where showSet s = '{':showSet' s
          showSet' []  = "}\n"
          showSet' [x] = show x ++"}\n"
          showSet' (x:xs) = show x ++ ", " ++ showSet' xs
