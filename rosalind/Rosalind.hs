module Rosalind where 
import Control.Arrow
import Data.Maybe
import Data.List.Split
import Data.List.Utils
import Data.List
import Data.Array
import System.IO
import Math.Combinatorics.Exact.Binomial
import Data.Tree
import Matrices
import Data.Tuple
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
editDistance s t = - globalAffineAlignmentScore (\x y->if x /= y then -1 else 0) 0 1 s t

editString :: String -> String -> (String, String)
editString s t = build (length s) (length t) [] []
  where a = fmap negate $ globalAffineAlignmentTable (\x y->if x /= y then -1 else 0) 0 1 s t
        build i j s' t'
          | i == 0 && j == 0                 = (s', t')
          | j == 0 || a!(i,j) == a!(i-1,j)+1 = build (i-1) j (u:s') ('-':t')
          | i == 0 || a!(i,j) == a!(i,j-1)+1 = build i (j-1) ('-':s') (v:t')
          | otherwise                        = build (i-1) (j-1) (u:s') (v:t')
          where u = s!!(i-1)
                v = t!!(j-1)

monoisotopicMassTable :: Array Char Double
monoisotopicMassTable = array ('A','Z') monoisotopicMassList
monoisotopicMassList :: [(Char,Double)]
monoisotopicMassList = zip proteinAlphabet [71.03711, 103.00919, 115.02694, 129.04259, 147.06841, 57.02146, 137.05891, 113.08406, 128.09496, 113.08406, 131.04049, 114.04293, 97.05276, 128.05858, 156.10111, 87.03203, 101.04768,99.06841,186.07931, 163.06333]
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
subTrees :: Tree a -> Forest a
subTrees (Node x sub) = Node x sub:concatMap subTrees sub

count :: (Eq a) => a -> [a] -> Int
count x = length . elemIndices x

lcs :: Eq a => [a] -> [a] -> [a]
lcs s t = reverse $ build n m
  where a = array ((0,0),(n,m)) [((x,y),f x y)|x<-[0..n],y<-[0..m]]
        n = length s
        m = length t
        s'= array (0,n-1) $ zip [0..n-1] s
        t'= array (0,m-1) $ zip [0..m-1] t
        f :: Int->Int->Int
        f i j
          | min i j == 0        = 0
          | s'!(i-1)==t'!(j-1)  = a!(i-1,j-1) +1
          | otherwise           = max (a!(i-1,j)) (a!(i,j-1))
        build i j 
          | min i j == 0          = []
          | s'!(i-1) == t'!(j-1)  = (s'!(i-1)):build (i-1) (j-1)
          | a!(i,j-1) > a!(i-1,j) = build i (j-1)
          | otherwise             = build (i-1) j

globalAffineAlignmentScore :: Eq a => (a -> a -> Int) -> Int -> Int -> [a] -> [a] -> Int
globalAffineAlignmentScore f g e s t = (!(length s,length t)) $ globalAffineAlignmentTable f g e s t

globalAffineAlignmentTable :: Eq a => (a->a->Int)->Int->Int->[a]->[a]->Array (Int,Int) Int
--subsitution, gap opening, gap extension, string 1, string 2
globalAffineAlignmentTable f g e s' t' = c
  where a = array ((0,0),(n,m)) [((x,y),a' x y)|x<-[0..n],y<-[0..m]] --s end with gap
        b = array ((0,0),(n,m)) [((x,y),b' x y)|x<-[0..n],y<-[0..m]] --t end with gap
        c = array ((0,0),(n,m)) [((x,y),c' x y)|x<-[0..n],y<-[0..m]] -- best 
        n = length s'
        m = length t'
        s= array (0,n-1) $ zip [0..n-1] s'
        t= array (0,m-1) $ zip [0..m-1] t'
        a' i j
          | i==0 || j==0  = inf
          | otherwise     = max (a!(i-1,j)-e) (c!(i-1,j)-g-e)
        b' i j
          | i==0 || j==0  = inf
          | otherwise     = max (b!(i,j-1)-e) (c!(i,j-1)-g-e)
        c' i j
          | max i j == 0  = 0
          | min i j == 0  = -g -e * max i j
          | otherwise     = maximum [b!(i,j),a!(i,j),match]
          where 
                match = c!(i-1,j-1) + f u v
                u = s!(i-1)
                v = t!(j-1)
        inf = -1073741824



localAffineAlignmentScore :: Eq a => (a -> a -> Int) -> Int -> Int -> [a] -> [a] -> Int
localAffineAlignmentScore f g e s t = fst $ maximum $ elems $ localAffineAlignmentTable f g e s t

localAffineAlignmentBest f g e s t = (drop si $ take ei s,drop sj $ take ej t,b1,b2)
  where a = localAffineAlignmentTable f g e s t
        (si,sj,b1,b2) = build ei ej [] []
        (ei,ej) = snd $ maximum $ map swap $ assocs a
        build x y s' t'
         | o == 0       = (x,y,s',t')
         | d == 1       = build (x-1) (y-1) (u:s') (v:t') 
         | d == 2       = build (x-1) y     (u:s') ('-':t') 
         | otherwise    = build x (y-1)     ('-':s') (v:t') 
         where (o,d) = a!(x,y)
               u     = s!!(x-1)
               v     = t!!(y-1)

localAffineAlignmentTable :: Eq a => (a->a->Int)->Int->Int->[a]->[a]->Array (Int,Int) (Int,Int)
--subsitution, gap opening, gap extension, string 1, string 2
localAffineAlignmentTable f g e s' t' = c
  where a = array ((0,0),(n,m)) [((x,y),a' x y)|x<-[0..n],y<-[0..m]] --s end with gap
        b = array ((0,0),(n,m)) [((x,y),b' x y)|x<-[0..n],y<-[0..m]] --t end with gap
        c = array ((0,0),(n,m)) [((x,y),c' x y)|x<-[0..n],y<-[0..m]] -- best 
        n = length s'
        m = length t'
        s= array (0,n-1) $ zip [0..n-1] s'
        t= array (0,m-1) $ zip [0..m-1] t'
        a' i j
          | i==0 || j==0  = inf
          | otherwise     = max (a!(i-1,j)-e) (fst (c!(i-1,j))-g-e)
        b' i j
          | i==0 || j==0  = inf
          | otherwise     = max (b!(i,j-1)-e) (fst (c!(i,j-1))-g-e)
        c' i j
          | min i j == 0  = (0,0)
          | otherwise     = maximum [(b!(i,j),3),(a!(i,j),2),(match,1),(0,0)]
          where 
                match = fst (c!(i-1,j-1)) + f u v
                u = s!(i-1)
                v = t!(j-1)
        inf = -1073741824
