import Data.List.Split
import Data.List.Utils
import Data.Maybe
import System.IO
import Rosalind
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
              then print result
              else do x <- getLine
                      loop (x:xs)
  where result = rnaTranslation' (last xs) (init xs)

rnaTranslation rna introns =  proteinTranslation $ dnaToRna $ concat $
                              foldl (flip concatMap) [rna] funcs
  where funcs = map deleteList introns
        deleteList x y 
         | isJust pos = s:deleteList x (drop (length x) e)
         | otherwise = [y]
         where pos = subIndex x y
               (s,e) = splitAt (fromJust pos) y

--This is a weaker version, since it's possible that certain substring
--are created after the removal
deleteList' x y
  | isJust pos = s++deleteList' x (drop (length x) e)
  | otherwise = y
  where pos = subIndex x y
        (s,e) = splitAt (fromJust pos) y
rnaTranslation' rna introns =  proteinTranslation $ dnaToRna $ 
                              foldl (\x y-> y x) rna funcs
  where funcs = map deleteList' introns
