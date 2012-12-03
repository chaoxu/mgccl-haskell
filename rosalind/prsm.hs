import System.IO
import Rosalind
import Data.List
import Data.Array 
main :: IO ()
main  = do n<-getLine 
           w<-getLines (read n::Int)
           z<-loop []
           print $ maximum $ zip (map (spectralConvolution z . completeSpectrum) w) w
           --print $ map (completeSpectrum) w
completeSpectrum xs = map weights $ filter (not . null) (tails xs++inits xs)
  where weights s = sum $ map (monoisotopicMassTable!) s
loop xs = do end <- isEOF
             if end
               then return xs
               else do c <- getLine
                       loop ((read c::Double):xs)

