main :: IO ()
main  = do n <- getLine
           print ((read n::Int)-2)
