--import Rosalind
--import Data.Function
import Data.Array
import Math.Combinatorics.Exact.Binomial

main :: IO ()
main  = do c <- getLine
           d <- getLine 
           let c' = map read (words c) :: [Integer]
               d' = map read (words d) :: [Integer]
           putStr $ unlines $ map (unwords . map show) (evil c' d')
  where evil [n,m] xs = [map (\x-> wrightFisherModel (2*n) (x) g ) xs| g<-[1..m]]
  --where evil [n,m] xs = [map (\x-> ((2*n),(2*n-x),g) ) xs| g<-[1..m]]

wrightFisherModel n m g =logBase 10 $ a!(g,0)
  where a = array ((0,0),(g,n)) [((i,j),d i j)|i<-[0..g],j<-[0..n]]
        d gen recessive
         | gen == 0  = if recessive == m then 1.0 else 0.0
         | otherwise = sum [(a!(gen-1,j))* binomial n recessive (p j)| j<-[0..n]]
         where p j = (fromInteger j) / (fromInteger n) 
--binomial :: (Floating a, Integral a1) =>  a1 -> a -> a1 -> a
binomial n k p =fromIntegral (n `choose` k)*(p** fromIntegral k)*((1-p)**fromIntegral (n-k))
