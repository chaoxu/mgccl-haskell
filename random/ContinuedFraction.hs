module ContinuedFraction where
import Data.Ratio
data (Integral a) => ContinuedFraction a = ContinuedFraction [a]
  deriving (Read,Show)
unContinuedFraction :: (Integral a) => ContinuedFraction a -> [a]
unContinuedFraction (ContinuedFraction s) = s

instance (Integral a) => Eq (ContinuedFraction a) where 
  (ContinuedFraction x) == (ContinuedFraction y)
    | k == lx && k == ly = True
    | k+2 == lx && k+1 == ly = f x y
    | k+2 == ly && k+1 == lx = f y x
    | otherwise = False
    where k = length (takeWhile (==True) $ zipWith (==) x y)
          lx = length $ take (k+2) x
          ly = length $ take (k+2) y
          f a b = (last a == 1) && last b - 1 == (a!!k)

instance (Integral a)=> Num (ContinuedFraction a) where
  (+)    = operate (0,1,1,0,1,0,0,0)
  (-)    = operate (0,1,-1,0,1,0,0,0)
  (*)    = operate (0,0,0,1,1,0,0,0)
  abs (ContinuedFraction x)    = ContinuedFraction (map abs x)
  negate (ContinuedFraction x) = ContinuedFraction (map negate x)
  fromInteger a = ContinuedFraction [fromInteger a]
  signum (ContinuedFraction [x]) = ContinuedFraction [signum x]
  signum (ContinuedFraction (x:y:xs)) 
    | x/=0 = ContinuedFraction [signum x]
    | otherwise = ContinuedFraction [signum y]

instance (Integral a) => Real (ContinuedFraction a) where
  toRational (ContinuedFraction [x]) = toInteger x % 1
  toRational (ContinuedFraction (x:xs)) = (toInteger x % 1) + ( (1%1) / toRational (ContinuedFraction xs))

instance (Integral a) => Fractional (ContinuedFraction a) where
  (/)    = operate (0,1,0,0,0,0,1,0)
  recip  = operate (0,1,0,0,0,0,1,0) (ContinuedFraction [1])
  fromRational x = ContinuedFraction (fromRational' x)
    where fromRational' x
            | r == 0 = [fromInteger q]
            | otherwise = fromInteger q:fromRational' (d%r)
            where (n,d) = (numerator x, denominator x)
                  (q,r) = quotRem n d

instance (Integral a) => RealFrac (ContinuedFraction a) where
  properFraction (ContinuedFraction (x:xs)) = (fromIntegral x,ContinuedFraction (0:xs))
  
  truncate (ContinuedFraction (x:xs)) = fromIntegral x
  
  round (ContinuedFraction [x,2]) = fromIntegral $ if even x then x else x+1
  round (ContinuedFraction [x,-2]) = fromIntegral $ if even x then x else x-1
  round x = floor $ x + ContinuedFraction [0,2]
  
  ceiling (ContinuedFraction [x]) = fromIntegral x
  ceiling (ContinuedFraction (x:y:xs))
    | x > 0 = fromIntegral (x+1)
    | x < 0 = fromIntegral x
  
  floor (ContinuedFraction [x]) = fromIntegral x
  floor (ContinuedFraction (x:y:xs))
    | x > 0 = fromIntegral x
    | x < 0 = fromIntegral (x-1)

instance (Integral a) => Ord (ContinuedFraction a) where
  (ContinuedFraction x) <= (ContinuedFraction y)
    | x == y = True
    | length (take (k+1) x) == k || length (take (k+1) y) == k = even k 
    | otherwise = ((-1)^k)*(x!!k - y!!k) < 0
    where k = length (takeWhile (==True) $ zipWith (==) x y)

operate :: (Integral a) => (a,a,a,a,a,a,a,a) -> ContinuedFraction a -> ContinuedFraction a -> ContinuedFraction a
operate z (ContinuedFraction x) (ContinuedFraction y) = ContinuedFraction (operate' z x y False)

operate' (_,_,_,_,0,0,0,0) _ _ _ = []
operate' (a,b,c,d,e,f,g,h) x y ydone
  | agree      = r: operate' (e, f, g, h, a-e*r, b-f*r, c-g*r, d-h*r) x y ydone
  | x/=[] && fromx      = operate' (b, a+b*p, d, c+d*p, f, e+f*p, h, g+h*p) (tail x) y ydone
  | x==[] && fromx      = operate' (b, b, d, d, f, f, h, h) [] y ydone
  | y/=[]              = operate' (c, d ,a+c*q, b+d*q, g, h, e+g*q, f+h*q) x (tail y) ydone
  | y==[]              = operate' (c, d ,c, d, g, h, g, h) x [] True
  where 
    agree = if any (==0) [e,f,g,h] 
            then False
            else all (==quot a e) [quot b f,quot c g,quot d h]
    p = head x
    q = head y
    r = quot a e
    fromx 
      | ydone = True
      | any (==0) [f,g,e,h] = False
      | otherwise = abs (b%f - a%e) > abs (c%g - a%e)
{--
operate'' (a,b,0,0) _ = []
operate'' (a,b,c,d) x
  | agree = q:operate'' (c, d, a-c*q, b-d*q) x
  | x==[] = operate'' (b,b,d,d) []
  | otherwise = operate'' (b,a+b*p,d,c+d*p) (tail x)
  where
    agree = if (any (==0) [c,d])
            then False
            else (quot a c) == (quot b d)
    q = quot a c
    p = head x

value  :: (Fractional a) => ContinuedFraction -> Int -> a
value (ContinuedFraction [x]) _    = fromIntegral x
value (ContinuedFraction (x:xs)) 1 = fromIntegral x
value (ContinuedFraction (x:xs)) n = (fromIntegral x) + 1 / (value (ContinuedFraction xs) (n-1))
--}
