module StepFunction ( StepFunction, eval, stepFunction, stepFunctionBase, toList) where

import Data.Monoid
data Bound a = Minimum | Value a | Maximum  deriving (Eq, Ord, Show)
type Interval a b = (Bound a, Bound a, b)
data StepFunction a b = StepFunction [Interval a b] 
    deriving (Show, Eq, Ord)

-- interval constructors
interval :: (Ord a) => a->a->b->Interval a b
interval a b y = (Value a, max (Value a) (Value b), y)
leftRay :: (Ord a) => a->b->Interval a b
leftRay a y = (Minimum, Value a, y)
rightRay :: (Ord a) => a->b->Interval a b
rightRay b y = (Value b, Maximum, y)

instance (Ord x, Eq y, Monoid y) => Monoid (StepFunction x y) where
  mempty = StepFunction [(Minimum, Maximum, mempty)]
  mappend (StepFunction xs) (StepFunction ys) = StepFunction (merge $ foldl insertInterval ys xs)
    where
      insertInterval [] _ = []
      insertInterval ((a',b',y'):ls) (a,b,y) 
       | a >= b' = non [(a',b',y')] ++ insertInterval ls (a,b,y) 
       | b >= b' = non [(a',a,y'),(a,b',y <> y')] ++ insertInterval ls (b',b,y) 
       | b <  b' = non [(a',a,y'),(a,b, y <> y'),(b,b',y')] ++ ls
       where non = filter (\(u,v,_)-> u/=v)
      merge (h@(a,_,y):h'@(_,b',y'):ls)
       | y == y'   = merge ((a,b',y):ls)
       | otherwise = h:merge (h':ls)
      merge x = x

eval :: (Ord a) => StepFunction a b -> a -> b
eval (StepFunction xs) t = y
  where (_,_,y) = head $ dropWhile sol xs
        sol (a,b,_)
         | a<=Value t && Value t<b = False
         | otherwise   = True

stepFunction :: (Ord a, Monoid b, Eq b) => [(a,a,b)] -> StepFunction a b
stepFunction = stepFunctionBase mempty

stepFunctionBase :: (Ord a, Monoid b, Eq b) => b -> [(a,a,b)] -> StepFunction a b
stepFunctionBase base xs = mconcat (map basis xs) `mappend` StepFunction [(Minimum, Maximum, base)]
  where basis (a,b,y) = StepFunction [leftRay (min a b) mempty, interval a b y, rightRay (max a b) mempty]

toList :: StepFunction a b -> (b,[(a,a,b)],b)
toList (StepFunction xs) = (value $ head xs, map (\(Value a, Value b, y)->(a,b,y)) $ reverse $ drop 1 $ reverse $ drop 1 xs,value $ last xs)
  where value (_,_,y) = y
