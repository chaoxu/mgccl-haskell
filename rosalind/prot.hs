import Rosalind
main :: IO ()
main  = do c <- getLine
           print $ head $ proteinTranscription c
