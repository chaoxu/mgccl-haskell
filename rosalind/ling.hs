import Data.SuffixTree
main :: IO ()
main  = do c <- getLine
           print $((fromIntegral $ countLength $ constructWith "ACGT" c)
                    /
                    (fromIntegral $ msc (length c)))
        where msc n = sum $ zipWith min [n,n-1..1] ((take 10 powers)++repeat 1000000)
              powers = 4: map (*4) powers

countLength (Node a) = (sum $ map (length.prefix.fst) a)+(sum $ map (countLength.snd) a)
countLength  Leaf    = 0
