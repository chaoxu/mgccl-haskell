import System.IO
import Data.List.Split
import Data.List
import Data.Char
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then do let ys = readGenBank' $ reverse xs
                       --print ys
                       putStr $ unlines $ writeFASTA ys (m ys) (n ys)
                       putStrLn ""
                       putStrLn $ writeNEXUS ys (m ys) (n ys)
                       putStr $ writePHYLIP ys (m ys) (n ys)
               else do c <- getLine
                       loop (c:xs)
    where n xs = maximum (map (length.snd) xs)
          m xs = maximum (map (length.fst) xs)

writeFASTA [] _ _ = []
writeFASTA ((x,y):xs) m n = ['>':x] ++ chunksOf 60 (take n (y ++ repeat 'N')) ++ writeFASTA xs m n

writeNEXUS xs m n = begin ++ w xs ++ end
  where begin = "#NEXUS\nbegin data;\n    dimensions ntax="++show (length xs)++" nchar="++show n++";\n    format datatype=dna missing=? gap=-;\nmatrix\n"
        end = ";\nend;\n"
        w [] = []
        w ((x,y):xs) = take (m+1) (x++repeat ' ')++(take n (y ++ repeat 'N'))++"\n"++ w xs

writePHYLIP xs m n = begin ++ (concatMap (\x->x++"\n") $ map unlines $ transpose list)
        where begin = " "++(show $ length xs)++" "++(show n)++"\n"
              list = map (\(x,y)-> zipWith (\a b-> b++a) (t y) ((take (10) (x++(repeat ' '))++" "):repeat (take (11) (repeat ' ')))) xs
              t x  = map unwords (chunksOf 5 $ chunksOf 10 $ take n (x ++ repeat 'N'))

readGenBank' xs = map (\[x,y]->(x,y)) (chunksOf 2 $ readGenBank xs)

readGenBank [] = []
readGenBank (x:xs)
  | null w        = readGenBank xs
  | head w == "LOCUS" = [head (tail w)] ++ readGenBank xs
  | head w == "ORIGIN" = [readGenData xs]++ readGenBank xs
  | otherwise         = readGenBank xs
  where w = words x
readGenData [] = []
readGenData (x:xs)
  | null w        = readGenData xs
  | head w == "//" = []
  | otherwise     = (concatMap (map toUpper) $ tail w)++readGenData xs
  where w = words x
