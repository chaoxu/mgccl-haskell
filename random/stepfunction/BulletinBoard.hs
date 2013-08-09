-- This solves the problem
-- http://mcicpc.cs.atu.edu/archives/2008/mcpc2008/bulletin/bulletin.html
-- It's also problem 4171 on ACM-ICPC Live Archive

import Data.Monoid
import StepFunction
import Data.List.Split
import Data.List

main :: IO ()
main = interact process

-- or you can use the Sum monoid, but that's too much pain..
instance Monoid Int where
  mempty = 0
  mappend a b = a+b

process :: String -> String
process input = sol l
  where l :: [Int]
        l = map read (words input)
        sol (n:w:h:xs) = solve w h (chunksOf 4 (take (n*4) xs)) ++ sol (drop (n*4) xs)
        sol _ = ""

-- solve one instance of the problem
solve :: Int->Int->[[Int]]->String
solve w h xs = show (area-ta) ++ " "++show md ++" " ++ show ma ++ "\n"
      where
        events :: [(Int,Int,Int,Int)]
        events = sort $ concatMap (\[xl,yl,xh,yh]->[(xl, yl, yh, 1),(xh, yl, yh, -1)]) xs
        area = w*h
        (ta,md,ma) = sweep (toStepFunction ([],[0])) 0 0 0 0 events
        sweep _ ta md ma _ [] = (ta, md, ma)
        sweep line ta md ma pre ((x,yl,yh,delta):xs) =
          sweep newline (ta+da) nmd nma x xs
          where width = x - pre
                newline = compress $ line <> toStepFunction ([yl,yh],[0,delta,0])
                da = height 1 line*width
                dma = height nmd line*width
                nmd = max md (maximum $ snd $ fromStepFunction newline)
                nma 
                 | nmd > md = 0
                 | otherwise = ma+dma

height x s = sum $ map (\(a,b,_)->b-a) $filter (\(_,_,y) -> y >= x) $ intervalNotation s
