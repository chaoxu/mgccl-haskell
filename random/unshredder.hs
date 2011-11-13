import Data.List
import Codec.Image.DevIL
import Data.Array.Unboxed
import Control.Applicative
import Data.Ord

main = do
  ilInit
  t <- readImage "file.png"
  --config the metric
  let metric = (sqrt.sqrt)
  let img = t
  let stripWidth = stripFind img metric
  let list = (decode img stripWidth)
  let result = concat (hash (unshred list metric) list)
  writeImage "unshredded.png" (encode result)

hash [] _      = []
hash x y       = foldr ((:).(y!!)) [] x

width img      = (w+1) where (_,w,_) = snd (bounds img)
height img     = (h+1) where (h,_,_) = snd (bounds img)
elemIndex' a b = (map (head . (flip elemIndices) b) a)

--parse the image into lists for easier manipulation
decode img stripWidth = toStrips' 0
  where 
  toStrips' start | start == (width img)  = []
                  | otherwise             = (toStrip2D start (start+stripWidth)):(toStrips' (start+stripWidth))

  toStrip2D x xend | x == xend   = []
                   | otherwise   = (toStrip1D x 0):(toStrip2D (x+1) xend)

  toStrip1D x y | y == (height img)  = []
                | otherwise          = ((color 0):(color 1):(color 2):[color 3]):(toStrip1D x (y+1))
                where color k  = (img!(y,x,k))

--convert the lists back to the image
encode :: [[[Word8]]] -> UArray (Int, Int, Int) Word8
encode list = array ((0,0,0),(ymax,xmax,3)) arr
             where arr = zip [(y,x,z)|(x,y,z)<-range((0,0,0),(xmax,ymax,3))] [x|z<-list,y<-z,x<-y]
                   xmax = (length list)-1
                   ymax = (length (head list))-1 

--function calculates the difference between two columns
diff xs ys metric = sum (map (uncurry deviation) (zip xs ys))
           where  deviation a b = distance (map fromIntegral a) (map fromIntegral b)
                  distance a b = (metric . sum) (map (\x -> (abs (fst x - snd x))) (zip a b))

--find best strip width
stripFind img metric = foldr1 gcd [divisors!!x |x<-top5]
              where list = map stripFind' divisors
                    top5 = take 5 (elemIndex' ((reverse.sort) list) list)
                    divisors = [n| n<-[2..(div (width img) 2)], mod (width img) n == 0]
                    stripFind' n = avg [diff (last (strips!!i)) (head (strips!!(i+1))) metric | i<-[0..((length strips)-2)]]
                         where strips  = (decode img n)
                               avg xs = (sum xs) / (fromIntegral $ length xs)
--unshred!
--unshred strips metric = unshredFrom ((length strips)-1)
unshred strips metric = minimumBy (comparing weight) [n:search bound n [n] | n<-[0..bound]]
  where
    --weight of each edge
    edgeWeight =  [ [(\a b -> diff (last a) (head b) metric) y x | x <- strips] | y <- strips]
    
    --sort the edges on each vertex by weight
    sortedEdges = map (uncurry elemIndex') (zip (map sort edgeWeight) edgeWeight)
    
    --search for the path
    search 0 _ _ = []
    search items n chosen = x:(search (items-1) x (x:chosen))
                          where x = head (dropWhile ((flip elem) chosen) (sortedEdges!!n))
    
    --Find weight of a certain path
    weight = maximum . map (uncurry (\x y-> edgeWeight!!x!!y)) . liftA2 zip init tail
    bound = (length strips)-1
