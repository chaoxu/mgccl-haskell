module Rosalind where 

import Data.Maybe
import Data.List.Split
import Data.List.Utils
import Data.List
import Data.Array
--this is for RNA
proteinTranslation :: String -> String
proteinTranslation x = map (fromJust . flip lookup t) $ chunksOf 3 x
    where t = map (\[a,b,c,d]->([a,b,c],d)) $ chunksOf 4 "UUUFCUULAUUIGUUVUUCFCUCLAUCIGUCVUUALCUALAUAIGUAVUUGLCUGLAUGMGUGVUCUSCCUPACUTGCUAUCCSCCCPACCTGCCAUCASCCAPACATGCAAUCGSCCGPACGTGCGAUAUYCAUHAAUNGAUDUACYCACHAACNGACDCAAQAAAKGAAECAGQAAGKGAGEUGUCCGURAGUSGGUGUGCCCGCRAGCSGGCGCGARAGARGGAGUGGWCGGRAGGRGGGGUGAXUAGXUAAX"
dnaToRna :: String -> String
dnaToRna = replace "T" "U"
reverseComplement :: String -> String
reverseComplement xs = map (replace "ACGT" "TGCA") (reverse xs)
  where replace::Eq a=>[a]->[a]->a->a
        replace a b x
         | isJust i  = b!!fromJust i
         | otherwise = x
         where i = elemIndex x a

proteinTranscription :: String -> [String]
proteinTranscription xs = filter (not . null) $ map (\x-> between $ drop x p) i
    where p = proteinTranslation xs 
          i = elemIndices 'M' p
          between xs
           | isJust end    = takeWhile (/='X') xs
           | otherwise     = []
           where end = elemIndex 'X' xs

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
