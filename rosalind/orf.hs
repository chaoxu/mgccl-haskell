import Data.List
import Rosalind
main :: IO ()
main  = do s <- getLine
           print $ dnaToProtein s

dnaToProtein dna = nub $ concatMap proteinTranscription l
    where l = [x,shift 1 x, shift 2 x, y, shift 1 y, shift 2 y]
          x = dnaToRna dna
          y = dnaToRna $ reverseComplement dna
          shift i s = reverse $ drop (3-i) $ reverse (drop i s)
