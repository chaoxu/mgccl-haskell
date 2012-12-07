import Bio.Alignment.AAlign
import Bio.Alignment.Matrices
import Bio.Sequence

main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ global_score blosum62 (-5,-5) (Seq (fromStr "") (fromStr c) Nothing) (Seq (fromStr "") (fromStr d) Nothing)

