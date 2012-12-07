import Bio.Alignment.AAlign
import Bio.Alignment.Matrices
import Bio.Sequence
--Not DONE
main :: IO ()
main  = do c <- getLine
           d <- getLine
           print $ global_score blosum62 (-11,-1) (Seq (fromStr "") (fromStr c) Nothing) (Seq (fromStr "") (fromStr d) Nothing)

