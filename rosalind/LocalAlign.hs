module LocalAlign where
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Array.ST hiding (unsafeFreeze)
import Control.Monad.ST
import Data.Tuple
type M = UArray (Int,Int) Int
type T s = STUArray s (Int,Int) Int

localAffineAlignment :: (Char -> Char -> Int)
                                    -> Int
                                    -> Int
                                    -> String
                                    -> String
                                    -> (Int, (String, String, String, String))
localAffineAlignment f g e s' t' = (score, best) where
    n = length s'
    m = length t'
    s= listArray (0,n-1) s' :: UArray Int Char
    t= listArray (0,m-1) t' :: UArray Int Char
    inf = -1073741824
    build ::T s -> T s -> T s -> T s -> (Int,Int) -> ST s ()
    build a b c d (i,j)
      | min i j == 0 = do
          writeArray a (i,j) inf
          writeArray b (i,j) inf
          writeArray c (i,j) 0
          writeArray d (i,j) 0
      | otherwise    = do
          o <- readArray a (i-1,j)
          p <- readArray c (i-1,j)
          writeArray a (i,j) $ max (o-e) (p-g-e)
          o' <- readArray b (i,j-1)
          p' <- readArray c (i,j-1)
          writeArray b (i,j) $ max (o'-e) (p'-g-e)
          y <- readArray a (i,j)
          z <- readArray b (i,j)
          x <- readArray c (i-1,j-1)
          let (c',d') = maximum [(z,3),(y,2),(x+f (s!(i-1)) (t!(j-1)),1),(0,0)]
          writeArray c (i,j) c'
          writeArray d (i,j) d'
    table :: (M,M)
    table   =  runST $ do
                 a <- newArray ((0,0),(n,m)) 0
                 b <- newArray ((0,0),(n,m)) 0
                 c <- newArray ((0,0),(n,m)) 0
                 d <- newArray ((0,0),(n,m)) 0
                 mapM_ (build a b c d) $ range ((0,0),(n,m))
                 o<-unsafeFreeze c
                 p<-unsafeFreeze d
                 return (o,p)

    score = maximum $ elems $ fst table

    best = (drop si $ take ei s',drop sj $ take ej t',b1,b2)
      where (a,d') = table
            (si,sj,b1,b2) = make ei ej [] []
            (ei,ej) = snd $ maximum $ map swap $ assocs a
            make x y ss tt
             | o == 0       = (x,y,ss,tt)
             | d == 1       = make (x-1) (y-1) (u:ss) (v:tt) 
             | d == 2       = make (x-1) y     (u:ss) ('-':tt) 
             | otherwise    = make x (y-1)     ('-':ss) (v:tt) 
             where o = a!(x,y)
                   d = d'!(x,y)
                   u = s!(x-1)
                   v = t!(y-1)
