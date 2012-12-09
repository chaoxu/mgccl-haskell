import System.IO
import Network.HTTP.Wget
import Data.List
import Control.Arrow hiding (loop)
main :: IO ()
main  = loop []

loop xs = do end <- isEOF
             if end
               then putStr $ format $ filter (\(x,_)->(not.null) x) $ map ((solve . snd) &&& fst) (reverse xs)
               else do d <- getLine
                       inFile <- wget ("http://www.uniprot.org/uniprot/"++d++".fasta") [] []
                       loop ((d, concat $ tail $ lines inFile):xs)
    where solve xs = filter (/=0) $ zipWith isGood (filter (\x->length x >= 4) $ map (take 4) $ tails xs) [1..]
          isGood [a,b,c,d] n = if a=='N' && (b/='P') && (c=='S' || c=='T') && (d/='P') then n else 0
          format xs = unlines [y++"\n"++unwords (map show x)|(x,y)<-xs]
