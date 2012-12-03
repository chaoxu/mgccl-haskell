import System.IO
import Rosalind
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then print $ map search $ reverse $ zipWith (-) (init xs) (tail xs)
               else do c <- getLine
                       loop ((read c::Double):xs)
  where search w = snd $ minimum $ map (\(x,y)->(abs (y-w),x)) monoisotopicMassList
