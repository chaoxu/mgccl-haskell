{-# LANGUAGE FlexibleInstances #-}
import System.IO
main :: IO ()
main  = loop [] "" ""

loop x v w = do end <- isEOF
                if end
                  then putStr $ show $ maximum ((comp v,w):x)
                  else do c <- getLine
                          if isName c
                            then loop ((comp v,w):x) [] (tail c)
                            else loop x (v++c) w
    where comp l = (fromIntegral $ length $ filter (\y-> y=='C' || y=='G') l)/ (fromIntegral $ length l)
          isName ('>':_) =True
          isName _ = False


