import Data.List.Split
import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           print $ takeWhile (/='X') $ map (fromJust . flip lookup t) $ chunksOf 3 c
t = map (\[a,b,c,d]->([a,b,c],d)) $ chunksOf 4 "UUUFCUULAUUIGUUVUUCFCUCLAUCIGUCVUUALCUALAUAIGUAVUUGLCUGLAUGMGUGVUCUSCCUPACUTGCUAUCCSCCCPACCTGCCAUCASCCAPACATGCAAUCGSCCGPACGTGCGAUAUYCAUHAAUNGAUDUACYCACHAACNGACDCAAQAAAKGAAECAGQAAGKGAGEUGUCCGURAGUSGGUGUGCCCGCRAGCSGGCGCGARAGARGGAGUGGWCGGRAGGRGGGGUGAXUAGXUAAX"