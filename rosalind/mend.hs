import Data.Maybe
import Newick
import Data.Tree
import Numeric.LinearAlgebra

main :: IO ()
main = do c <- getLine
          putStrLn $ unwords $ map show (toList $ solve $ fmap fst $ newickTree c :: [Double])
    where solve (Node x forest)
           | x == "AA" = fromList [1.0,0.0,0.0]
           | x == "Aa" = fromList [0.0,1.0,0.0]
           | x == "aa" = fromList [0.0,0.0,1.0]
           | otherwise = prob (map solve forest)
            where prob [u,v] = sum $ zipWith scale (toList u) $ map (<>v) [mAA,mAa,maa]
          mAA = trans $ (3><3) [1.0,0.0,0.0,0.5,0.5,0.0,0.0,1.0,0.0]
          mAa = trans $ (3><3) [0.5,0.5,0.0,0.25,0.5,0.25,0.0,0.5,0.5]
          maa = trans $ (3><3) [0.0,1.0,0.0,0.0,0.5,0.5,0.0,0.0,1.0]
