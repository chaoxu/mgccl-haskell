import Data.List
import Codec.Image.DevIL
import Data.Array.Unboxed
import Control.Applicative
import Data.Ord

--config the metric
metric = (sqrt.sqrt)

main = do
  ilInit
  t <- readImage "file.png"
  let img = t
  let stripWidth = stripFind img
  let list = (decode img stripWidth)
  let result = concat (hash (unshred list) list)
  writeImage "unshredded.png" (encode (snd $ bounds img) result)

hash x y       = foldr ((:).(y!!)) [] x
width img      = (w+1) where (_,w,_) = snd $ bounds img
height img     = (h+1) where (h,_,_) = snd $ bounds img
elemIndex' a b = map (head . (flip elemIndices) b) a

--convert the img into lists
decode img stripWidth = chunk stripWidth [[[img!(y,x,z)|z<-[0..3]]|y<-[0..((height img)-1)]]|x<-[0..((width img)-1)]]
                      where chunk n = takeWhile (not.null) . map (take n) . iterate (drop n)

--convert the lists back to the image
encode :: (Int,Int,Int) -> [[[Word8]]] ->UArray (Int, Int, Int) Word8
encode bound list = array ((0,0,0),bound) (zip [(y,x,z)|(y,x,z)<-range((0,0,0),bound)] [y|z<-(transpose list),x<-z,y<-x])

--function calculates the difference between two columns
diff xs ys = sum $ map (uncurry distance) (zip (last xs) (head ys))
           where distance a b = metric $ sum $ zipWith ((abs .) . (-)) (map fromIntegral a) (map fromIntegral b)

--find best strip width
stripFind img = foldr1 gcd [divisors!!x |x<-top5]
              where list = map stripFind' divisors
                    top5 = take 5 (elemIndex' ((reverse.sort) list) list)
                    divisors = [n| n<-[2..(div (width img) 2)], mod (width img) n == 0]
                    stripFind' n = avg $ zipWith diff (init strips) (tail strips)
                         where strips  = decode img n
                               avg xs = (sum xs) / (fromIntegral $ length xs)
--unshred!
unshred strips = minimumBy (comparing weight) [n:search bound n [n] | n<-[0..bound]]
  where
    --weight of each edge
    edgeWeight =  [ [diff y x | x <- strips] | y <- strips]
    
    --sort the edges on each vertex by weight
    sortedEdges = zipWith elemIndex' (map sort edgeWeight) edgeWeight
    
    --search for the path
    search 0 _ _ = []
    search items n chosen = x:search (items-1) x (x:chosen)
                          where x = head $ dropWhile (flip elem $ chosen) (sortedEdges!!n)
    
    --Find weight of a certain path
    weight = maximum . (<*>) (zipWith (\x y-> edgeWeight!!x!!y) . init) tail
    bound = (length strips)-1
