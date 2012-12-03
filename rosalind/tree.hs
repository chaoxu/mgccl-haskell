import System.IO

main :: IO ()
main  = do ns <- getLine
           t <- loop 0
           print ((read ns :: Int)-t-1)
loop n = do end <- isEOF
            if end
               then return n
               else do getLine
                       loop (n+1)
