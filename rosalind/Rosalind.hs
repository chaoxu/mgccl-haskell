module Rosalind where 
import Control.Arrow
import Data.Maybe
import Data.List.Split
import Data.List.Utils
import Data.List
import Data.Array
import System.IO
import Math.Combinatorics.Exact.Binomial
--this is for RNA
proteinTranslation :: String -> String
proteinTranslation x = map (fromJust . flip lookup t) $ chunksOf 3 x
    where t = map (\[a,b,c,d]->([a,b,c],d)) $ chunksOf 4 rnaProteinTable

rnaProteinTable :: String
rnaProteinTable = "UUUFCUULAUUIGUUVUUCFCUCLAUCIGUCVUUALCUALAUAIGUAVUUGLCUGLAUGMGUGVUCUSCCUPACUTGCUAUCCSCCCPACCTGCCAUCASCCAPACATGCAAUCGSCCGPACGTGCGAUAUYCAUHAAUNGAUDUACYCACHAACNGACDCAAQAAAKGAAECAGQAAGKGAGEUGUCCGURAGUSGGUGUGCCCGCRAGCSGGCGCGARAGARGGAGUGGWCGGRAGGRGGGGUGAXUAGXUAAX"
dnaToRna :: String -> String
dnaToRna = replace "T" "U"
reverseComplement :: String -> String
reverseComplement xs = map (rep "ACGT" "TGCA") (reverse xs)
  where rep::Eq a=>[a]->[a]->a->a
        rep a b x
         | isJust i  = b!!fromJust i
         | otherwise = x
         where i = elemIndex x a

proteinTranscription :: String -> [String]
proteinTranscription xs = filter (not . null) $ map (\x-> between $ drop x p) i
    where p = proteinTranslation xs 
          i = elemIndices 'M' p
          between l
           | isJust end    = takeWhile (/='X') l
           | otherwise     = []
           where end = elemIndex 'X' l

editDistance :: Eq a => [a] -> [a] -> Int
editDistance s t = editTable s t!(length s,length t)

editTable :: Eq a => [a] -> [a] -> Array (Int, Int) Int
editTable s t = a
  where a = array ((0,0),(n,m)) [((x,y),f x y)|x<-[0..n],y<-[0..m]]
        n = length s
        m = length t
        f i j
          | min i j == 0        = max i j
          | otherwise           = minimum [x,y,z]
          where 
                x = 1+a!(i-1,j)
                y = 1+a!(i,j-1)
                z = a!(i-1,j-1)+(if u==v then 0 else 1)
                u = s!!(i-1)
                v = t!!(j-1)
--Gap distance function, subsitution cost 
scoringEditDistance :: Eq a => (Int->Int)->(a->a->Int)->[a]->[a]->Int
editString :: String -> String -> (String, String)
editString s t = build (length s) (length t) [] []
  where a = editTable s t
        build i j s' t'
          | i == 0 && j == 0                 = (s', t')
          | j == 0 || a!(i,j) == a!(i-1,j)+1 = build (i-1) j (u:s') ('-':t')
          | i == 0 || a!(i,j) == a!(i,j-1)+1 = build i (j-1) ('-':s') (v:t')
          | otherwise                        = build (i-1) (j-1) (u:s') (v:t')
          where u = s!!(i-1)
                v = t!!(j-1)

monoiZsotopicMassTable :: Array Char Double
monoisotopicMassTable = array ('A','Z') monoisotopicMassList
monoisotopicMassList :: [(Char,Double)]
monoisotopicMassList = zip "ACDEFGHIKLMNPQRSTVWY" [71.03711, 103.00919, 115.02694, 129.04259, 147.06841, 57.02146, 137.05891, 113.08406, 128.09496, 113.08406, 131.04049, 114.04293, 97.05276, 128.05858, 156.10111, 87.03203, 101.04768,99.06841,186.07931, 163.06333]
proteinFromMass :: Double -> Maybe Char
proteinFromMass w 
  | e < epsilon = Just r
  | otherwise = Nothing
  where (e,r) = proteinFromMassError w
        epsilon = 0.00002
proteinFromMassError :: Double -> (Double, Char)
proteinFromMassError w = minimum $ map (\(x,y)->(abs (y-w),x)) monoisotopicMassList
readFASTA :: IO (String, String)
readFASTA = do name <- getLine
               dna  <- readData
               return (tail name,dna)
  where readData = do end <- isEOF
                      if end
                        then return ""
                        else do c<-hLookAhead stdin
                                if c /='>'
                                then do x <- getLine
                                        y <- readData
                                        return (x++y)
                                 else return ""

hammingDistance :: Eq b => [b] -> [b] -> Int
hammingDistance x y = length $ filter (==False) $ zipWith (==) x y

pDistance :: (Eq a1, Fractional a) => [a1] -> [a1] -> a
pDistance x y = fromIntegral (hammingDistance x y)/ fromIntegral (length x)

spectralConvolution :: (Fractional c', Ord c') => [c'] -> [c'] -> (Int, c')
spectralConvolution a b = maximum $ map (length &&& head) $ groupBy close $ sort [x-y|x<-a,y<-b]
      where close x y = abs (x-y) < 0.0000001
getLines :: (Eq a, Num a) => a -> IO [String]
getLines 0 = return []
getLines k = do c <- getLine
                d <- getLines (k-1)
                return $ c:d

--find all the edges on the deBruijin graph
deBruijn :: [String] -> [(String, String)]
deBruijn xs = nub $ map (init &&& tail) $ nub (xs++map reverseComplement xs)
deBruijnString :: Eq t => [t] -> [t] -> [([t], [t])] -> [t]
deBruijnString s x e
            | x == s    = [head s]
            | otherwise = head x:deBruijnString s (fromJust (lookup x e)) e
binomial :: (Floating a, Integral a1) =>  a1 -> a -> a1 -> a
binomial n p k =fromIntegral (n `choose` k)*(p** fromIntegral k)*((1-p)**fromIntegral (n-k))
